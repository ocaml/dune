open Stdune
open Fiber.O

module Scheduler = struct
  type t =
    { on_event : Fiber.fill -> unit
    ; register_pending_ivar : unit -> unit
    ; spawn_thread : (unit -> unit) -> unit
    }
end

module Async : sig
  type t

  val create : Scheduler.t -> t

  val task : t -> f:(unit -> 'a) -> 'a Or_exn.t Fiber.t

  val task_exn : t -> f:(unit -> 'a) -> 'a Fiber.t

  val stop : t -> unit
end = struct
  type task = Task : 'a Or_exn.t Fiber.Ivar.t * (unit -> 'a) -> task

  type t =
    { worker : task Worker.t
    ; scheduler : Scheduler.t
    }

  let stop t = Worker.stop t.worker

  let create (scheduler : Scheduler.t) =
    let do_ (Task (ivar, f)) =
      let res = Result.try_with f in
      scheduler.on_event (Fiber.Fill (ivar, res))
    in
    let worker = Worker.create ~spawn:scheduler.spawn_thread do_ in
    { worker; scheduler }

  let task (t : t) ~f =
    t.scheduler.register_pending_ivar ();
    let ivar = Fiber.Ivar.create () in
    match Worker.add_work t.worker (Task (ivar, f)) with
    | Ok () -> Fiber.Ivar.read ivar
    | Error `Stopped -> Code_error.raise "worker stopped" []

  let task_exn t ~f =
    let+ res = task t ~f in
    match res with
    | Ok s -> s
    | Error e -> raise e
end

module Session_id = Id.Make ()

let debug = Option.is_some (Env.get Env.initial "DUNE_RPC_DEBUG")

module Session = struct
  module Id = Session_id

  type t =
    { out_channel : out_channel
    ; in_channel : in_channel
    ; id : Id.t
    ; writer : Async.t
    ; reader : Async.t
    ; scheduler : Scheduler.t
    }

  let create in_channel out_channel scheduler =
    if debug then Format.eprintf ">> NEW SESSION@.";
    let reader_ref = ref None in
    let t =
      let id = Id.gen () in
      { in_channel
      ; out_channel
      ; id
      ; reader = Async.create scheduler
      ; writer = Async.create scheduler
      ; scheduler
      }
    in
    reader_ref := Some t.reader;
    t

  let string_of_packet = function
    | None -> "EOF"
    | Some csexp -> Csexp.to_string csexp

  let read t =
    let rec read () =
      try Csexp.input_opt t.in_channel with
      | Unix.Unix_error (EBADF, _, _) -> Ok None
      | Sys_error _ -> Ok None
      | Sys_blocked_io -> read ()
      | e -> reraise e
    in
    let+ res = Async.task t.reader ~f:read in
    let res =
      match res with
      | Error exn ->
        Async.stop t.reader;
        raise exn
      | Ok res -> (
        match res with
        | Ok (Some _ as s) -> s
        | Error _
        | Ok None ->
          Async.stop t.reader;
          None )
    in
    if debug then Format.eprintf "<< %s@." (string_of_packet res);
    res

  let write t sexp =
    if debug then Format.eprintf ">> %s@." (string_of_packet sexp);
    match sexp with
    | Some sexp ->
      Async.task_exn t.writer ~f:(fun () ->
          Csexp.to_channel t.out_channel sexp;
          flush t.out_channel)
    | None ->
      Async.task_exn t.writer ~f:(fun () ->
          close_in_noerr t.in_channel;
          close_out_noerr t.out_channel)
end

let close_fd_no_error fd = try Unix.close fd with _ -> ()

type where =
  [ `Unix of Path.t
  | `Ip of [ `Ipv4 | `Ipv6 ] * Unix.inet_addr * [ `Port of int ]
  ]

let domain = function
  | `Unix _ -> Unix.PF_UNIX
  | `Ip (`Ipv4, _, _) -> Unix.PF_INET
  | `Ip (`Ipv6, _, _) -> Unix.PF_INET6

let sockaddr = function
  | `Unix p -> Unix.ADDR_UNIX (Path.to_string p)
  | `Ip (_, addr, `Port port) -> Unix.ADDR_INET (addr, port)

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; where : where
      ; r_interrupt_accept : Unix.file_descr
      ; w_interrupt_accept : Unix.file_descr
      ; buf : Bytes.t
      }

    let create where ~backlog =
      let fd = Unix.socket (domain where) Unix.SOCK_STREAM 0 in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.set_nonblock fd;
      ( match where with
      | `Unix p ->
        Path.unlink_no_err p;
        Path.mkdir_p (Path.parent_exn p)
      | `Ip _ -> () );
      Unix.bind fd (sockaddr where);
      Unix.listen fd backlog;
      let r_interrupt_accept, w_interrupt_accept = Unix.pipe () in
      Unix.set_nonblock r_interrupt_accept;
      let buf = Bytes.make 1 '0' in
      { fd; where; r_interrupt_accept; w_interrupt_accept; buf }

    let rec accept t =
      match Unix.select [ t.r_interrupt_accept; t.fd ] [] [] (-1.0) with
      | r, [], [] ->
        let inter, accept =
          List.fold_left r ~init:(false, false) ~f:(fun (i, a) fd ->
              if fd = t.fd then
                (i, true)
              else if fd = t.r_interrupt_accept then
                (true, a)
              else
                assert false)
        in
        if inter then
          None
        else if accept then
          let fd, _ = Unix.accept t.fd in
          Some fd
        else
          assert false
      | _, _, _ -> assert false
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> accept t
      | exception Unix.Unix_error (Unix.EBADF, _, _) -> None

    let stop t =
      let _ = Unix.write t.w_interrupt_accept t.buf 0 1 in
      close_fd_no_error t.fd;
      match t.where with
      | `Unix p -> Path.unlink_no_err p
      | `Ip _ -> ()
  end

  type t =
    { mutable transport : Transport.t option
    ; backlog : int
    ; scheduler : Scheduler.t
    ; where : where
    }

  let create where ~backlog scheduler =
    { where; backlog; scheduler; transport = None }

  let serve (t : t) =
    let async = Async.create t.scheduler in
    let+ transport =
      Async.task_exn async ~f:(fun () ->
          Transport.create t.where ~backlog:t.backlog)
    in
    t.transport <- Some transport;
    let accept () =
      Async.task async ~f:(fun () ->
          Transport.accept transport
          |> Option.map ~f:(fun client ->
                 let in_ = Unix.in_channel_of_descr client in
                 let out = Unix.out_channel_of_descr client in
                 (in_, out)))
    in
    let loop () =
      let+ accept = accept () in
      match accept with
      | Error _
      | Ok None ->
        None
      | Ok (Some (in_, out)) ->
        let session = Session.create in_ out t.scheduler in
        Some session
    in
    Fiber.Stream.In.create loop

  let stop t =
    match t.transport with
    | None -> Code_error.raise "server not running" []
    | Some t -> Transport.stop t
end

module Client = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; where : where
      }

    let close t = close_fd_no_error t.fd

    let create where =
      let fd = Unix.socket (domain where) Unix.SOCK_STREAM 0 in
      { where; fd }

    let connect t =
      let () = Unix.connect t.fd (sockaddr t.where) in
      t.fd
  end

  type t =
    { mutable transport : Transport.t option
    ; async : Async.t
    ; scheduler : Scheduler.t
    ; where : where
    }

  let create where scheduler =
    let async = Async.create scheduler in
    { where; scheduler; async; transport = None }

  let connect t =
    Async.task_exn t.async ~f:(fun () ->
        let transport = Transport.create t.where in
        t.transport <- Some transport;
        let client = Transport.connect transport in
        let out = Unix.out_channel_of_descr client in
        let in_ = Unix.in_channel_of_descr client in
        Session.create in_ out t.scheduler)

  let stop t = Option.iter t.transport ~f:Transport.close
end
