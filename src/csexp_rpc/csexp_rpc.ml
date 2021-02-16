open Stdune
open Fiber.O

module Scheduler = struct
  type t =
    { create_thread_safe_ivar : 'a. unit -> 'a Fiber.Ivar.t * ('a -> unit)
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
  type t =
    { worker : Worker.t
    ; scheduler : Scheduler.t
    }

  let stop t = Worker.stop t.worker

  let create (scheduler : Scheduler.t) =
    let worker = Worker.create ~spawn_thread:scheduler.spawn_thread in
    { worker; scheduler }

  let task (t : t) ~f =
    let ivar, fill = t.scheduler.create_thread_safe_ivar () in
    let f () = fill (Result.try_with f) in
    match Worker.add_work t.worker ~f with
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
    Async.task_exn t.writer
      ~f:
        ( match sexp with
        | Some sexp ->
          fun () ->
            Csexp.to_channel t.out_channel sexp;
            flush t.out_channel
        | None ->
          fun () ->
            close_in_noerr t.in_channel;
            close_out_noerr t.out_channel )
end

let close_fd_no_error fd = try Unix.close fd with _ -> ()

module Address = struct
  type ip =
    | V4
    | V6

  type port = int

  type t =
    | Unix of Path.t
    | Ip of ip * Unix.inet_addr * port

  let domain = function
    | Unix _ -> Unix.PF_UNIX
    | Ip (V4, _, _) -> Unix.PF_INET
    | Ip (V6, _, _) -> Unix.PF_INET6

  let sockaddr = function
    | Unix p -> Unix.ADDR_UNIX (Path.to_string p)
    | Ip (_, addr, port) -> Unix.ADDR_INET (addr, port)

  let of_sockaddr sockaddr =
    match sockaddr with
    | Unix.ADDR_UNIX p -> Unix (Path.of_string p)
    | Unix.ADDR_INET (addr, port) ->
      let ip =
        match Unix.domain_of_sockaddr sockaddr with
        | PF_UNIX -> assert false
        | PF_INET -> V4
        | PF_INET6 -> V6
      in
      Ip (ip, addr, port)
end

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; address : Address.t
      ; r_interrupt_accept : Unix.file_descr
      ; w_interrupt_accept : Unix.file_descr
      ; buf : Bytes.t
      }

    let create address ~backlog =
      let fd = Unix.socket (Address.domain address) Unix.SOCK_STREAM 0 in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.set_nonblock fd;
      ( match address with
      | Unix p ->
        Path.unlink_no_err p;
        Path.mkdir_p (Path.parent_exn p)
      | Ip _ -> () );
      Unix.bind fd (Address.sockaddr address);
      Unix.listen fd backlog;
      let r_interrupt_accept, w_interrupt_accept = Unix.pipe () in
      Unix.set_nonblock r_interrupt_accept;
      let buf = Bytes.make 1 '0' in
      { fd; address; r_interrupt_accept; w_interrupt_accept; buf }

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
      match t.address with
      | Unix p -> Path.unlink_no_err p
      | Ip _ -> ()
  end

  type t =
    { mutable transport : Transport.t option
    ; backlog : int
    ; scheduler : Scheduler.t
    ; address : Address.t
    }

  let create address ~backlog scheduler =
    { address; backlog; scheduler; transport = None }

  let serve (t : t) =
    let async = Async.create t.scheduler in
    let+ transport =
      Async.task_exn async ~f:(fun () ->
          Transport.create t.address ~backlog:t.backlog)
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

  let listening_address t =
    match t.transport with
    | None -> Code_error.raise "server not running" []
    | Some t -> Address.of_sockaddr (Unix.getsockname t.fd)
end

module Client = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; address : Address.t
      }

    let close t = close_fd_no_error t.fd

    let create address =
      let fd = Unix.socket (Address.domain address) Unix.SOCK_STREAM 0 in
      { address; fd }

    let connect t =
      let () = Unix.connect t.fd (Address.sockaddr t.address) in
      t.fd
  end

  type t =
    { mutable transport : Transport.t option
    ; async : Async.t
    ; scheduler : Scheduler.t
    ; address : Address.t
    }

  let create address scheduler =
    let async = Async.create scheduler in
    { address; scheduler; async; transport = None }

  let connect t =
    Async.task_exn t.async ~f:(fun () ->
        let transport = Transport.create t.address in
        t.transport <- Some transport;
        let client = Transport.connect transport in
        let out = Unix.out_channel_of_descr client in
        let in_ = Unix.in_channel_of_descr client in
        Session.create in_ out t.scheduler)

  let stop t = Option.iter t.transport ~f:Transport.close
end
