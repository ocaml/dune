open Stdune
open Fiber.O

module Worker = struct
  include Dune_engine.Scheduler.Worker

  let task_exn t ~f =
    let+ res = task t ~f in
    match res with
    | Error `Stopped -> assert false
    | Error (`Exn e) -> Exn_with_backtrace.reraise e
    | Ok s -> s
end

module Session_id = Id.Make ()

let debug = Option.is_some (Env.get Env.initial "DUNE_RPC_DEBUG")

module Session = struct
  module Id = Session_id

  type kind =
    | Socket
    | Channel

  type t =
    { out_channel : out_channel
    ; in_channel : in_channel
    ; id : Id.t
    ; mutable writer : Worker.t option
    ; mutable reader : Worker.t option
    ; kind : kind
    }

  let create_full kind in_channel out_channel =
    if debug then Format.eprintf ">> NEW SESSION@.";
    let reader_ref = ref None in
    let+ t =
      let* reader = Worker.create () in
      let+ writer = Worker.create () in
      let id = Id.gen () in
      { in_channel
      ; out_channel
      ; id
      ; reader = Some reader
      ; writer = Some writer
      ; kind
      }
    in
    reader_ref := Some t.reader;
    t

  let create in_channel out_channel = create_full Channel in_channel out_channel

  let string_of_packet = function
    | None -> "EOF"
    | Some csexp -> Csexp.to_string csexp

  let read t =
    let debug res =
      if debug then Format.eprintf "<< %s@." (string_of_packet res)
    in
    match t.reader with
    | None ->
      debug None;
      Fiber.return None
    | Some reader ->
      let rec read () =
        try Csexp.input_opt t.in_channel with
        | Unix.Unix_error (_, _, _) -> Ok None
        | Sys_error _ -> Ok None
        | Sys_blocked_io -> read ()
        | e -> reraise e
      in
      let+ res = Worker.task reader ~f:read in
      let res =
        match res with
        | Error (`Exn _)
        | Error `Stopped ->
          Worker.stop reader;
          t.reader <- None;
          None
        | Ok res -> (
          match res with
          | Ok (Some _ as s) -> s
          | Error _
          | Ok None ->
            Worker.stop reader;
            t.reader <- None;
            None)
      in
      debug res;
      res

  let write t sexp =
    if debug then Format.eprintf ">> %s@." (string_of_packet sexp);
    match t.writer with
    | None ->
      Code_error.raise "attempting to write to a closed channel"
        [ ("sexp", (Dyn.Encoder.option Sexp.to_dyn) sexp) ]
    | Some writer -> (
      let+ res =
        Worker.task writer
          ~f:
            (match sexp with
            | Some sexp ->
              fun () ->
                Csexp.to_channel t.out_channel sexp;
                flush t.out_channel
            | None -> (
              match t.kind with
              | Channel -> fun () -> close_out_noerr t.out_channel
              | Socket -> (
                fun () ->
                  let fd = Unix.descr_of_out_channel t.out_channel in
                  try Unix.shutdown fd Unix.SHUTDOWN_SEND with
                  | Unix.Unix_error _ -> ())))
      in
      match res with
      | Ok () -> ()
      | Error `Stopped -> assert false
      | Error (`Exn e) ->
        t.writer <- None;
        Worker.stop writer;
        Exn_with_backtrace.reraise e)
end

let close_fd_no_error fd =
  try Unix.close fd with
  | _ -> ()

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      ; r_interrupt_accept : Unix.file_descr
      ; w_interrupt_accept : Unix.file_descr
      ; buf : Bytes.t
      }

    let create sockaddr ~backlog =
      let fd =
        Unix.socket ~cloexec:true
          (Unix.domain_of_sockaddr sockaddr)
          Unix.SOCK_STREAM 0
      in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      Unix.set_nonblock fd;
      (match sockaddr with
      | ADDR_UNIX p ->
        let p = Path.of_string p in
        Path.unlink_no_err p;
        Path.mkdir_p (Path.parent_exn p);
        at_exit (fun () -> Path.unlink_no_err p)
      | _ -> ());
      Unix.bind fd sockaddr;
      Unix.listen fd backlog;
      let r_interrupt_accept, w_interrupt_accept = Unix.pipe () in
      Unix.set_nonblock r_interrupt_accept;
      let buf = Bytes.make 1 '0' in
      { fd; sockaddr; r_interrupt_accept; w_interrupt_accept; buf }

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
          let fd, _ = Unix.accept ~cloexec:true t.fd in
          Some fd
        else
          assert false
      | _, _, _ -> assert false
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> accept t
      | exception Unix.Unix_error (Unix.EBADF, _, _) -> None

    let stop t =
      let _ = Unix.write t.w_interrupt_accept t.buf 0 1 in
      close_fd_no_error t.fd;
      match t.sockaddr with
      | ADDR_UNIX p -> Fpath.unlink_no_err p
      | _ -> ()
  end

  type t =
    { mutable transport : Transport.t option
    ; backlog : int
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr ~backlog = { sockaddr; backlog; transport = None }

  let serve (t : t) =
    let* async = Worker.create () in
    let+ transport =
      Worker.task_exn async ~f:(fun () ->
          Transport.create t.sockaddr ~backlog:t.backlog)
    in
    t.transport <- Some transport;
    let accept () =
      Worker.task async ~f:(fun () ->
          Transport.accept transport
          |> Option.map ~f:(fun client ->
                 let in_ = Unix.in_channel_of_descr client in
                 let out = Unix.out_channel_of_descr client in
                 (in_, out)))
    in
    let loop () =
      let* accept = accept () in
      match accept with
      | Error _
      | Ok None ->
        Fiber.return None
      | Ok (Some (in_, out)) ->
        let+ session = Session.create_full Socket in_ out in
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
    | Some t -> Unix.getsockname t.fd
end

module Client = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      }

    let close t = close_fd_no_error t.fd

    let create sockaddr =
      let fd =
        Unix.socket ~cloexec:true
          (Unix.domain_of_sockaddr sockaddr)
          Unix.SOCK_STREAM 0
      in
      { sockaddr; fd }

    let connect t =
      let () = Unix.connect t.fd t.sockaddr in
      t.fd
  end

  type t =
    { mutable transport : Transport.t option
    ; async : Worker.t
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr =
    let+ async = Worker.create () in
    { sockaddr; async; transport = None }

  let connect t =
    let* in_, out =
      Worker.task_exn t.async ~f:(fun () ->
          let transport = Transport.create t.sockaddr in
          t.transport <- Some transport;
          let client = Transport.connect transport in
          let out = Unix.out_channel_of_descr client in
          let in_ = Unix.in_channel_of_descr client in
          (in_, out))
    in
    Session.create in_ out

  let stop t = Option.iter t.transport ~f:Transport.close
end
