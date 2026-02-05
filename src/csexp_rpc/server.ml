open Stdune
open Fiber.O
open Dune_scheduler

module Transport = struct
  type t =
    { sockets : (Unix.sockaddr * Unix.file_descr) list
    ; mutable task : (Unix.file_descr * Unix.sockaddr) Async_io.Task.t option
    ; mutable running : bool
    }

  let create sockets ~backlog =
    List.iter sockets ~f:(fun (_, fd) ->
      Unix.listen fd backlog;
      Unix.set_nonblock fd);
    { sockets; task = None; running = true }
  ;;

  let close t =
    let+ () = Fiber.parallel_iter ~f:(fun (_, fd) -> Async_io.close fd) t.sockets in
    Ok None
  ;;

  let rec accept t =
    let* () = Fiber.return () in
    match t.running with
    | false -> close t
    | true ->
      let* task =
        Async_io.ready_one t.sockets `Read ~f:(fun _ fd -> Unix.accept ~cloexec:true fd)
      in
      t.task <- Some task;
      let* res = Async_io.Task.await task in
      (match res with
       | Error (`Exn (Unix.Unix_error (Unix.EAGAIN, _, _))) -> accept t
       | Error (`Exn exn) ->
         let+ _ = close t in
         Error (Exn_with_backtrace.capture exn)
       | Error `Cancelled -> close t
       | Ok (fd, _) ->
         Socket.maybe_set_nosigpipe fd;
         Unix.set_nonblock fd;
         Fiber.return @@ Ok (Some fd))
  ;;

  let stop t =
    let* () = Fiber.return () in
    t.running <- false;
    let+ () =
      match t.task with
      | None -> Fiber.return ()
      | Some task -> Async_io.Task.cancel task
    in
    List.iter t.sockets ~f:(fun (addr, _) ->
      match (addr : Unix.sockaddr) with
      | ADDR_UNIX p -> Fpath.unlink_no_err p
      | _ -> ())
  ;;
end

type t =
  { mutable state : [ `Init of Unix.file_descr list | `Running of Transport.t | `Closed ]
  ; backlog : int
  ; sockaddrs : Unix.sockaddr list
  ; ready : unit Fiber.Ivar.t
  }

let create sockaddrs ~backlog =
  try
    let fds =
      List.map sockaddrs ~f:(fun sockaddr ->
        let fd =
          Unix.socket ~cloexec:true (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
        in
        Unix.set_nonblock fd;
        Unix.setsockopt fd Unix.SO_REUSEADDR true;
        Socket.bind fd sockaddr;
        fd)
    in
    Ok { sockaddrs; backlog; state = `Init fds; ready = Fiber.Ivar.create () }
  with
  | Unix.Unix_error (EADDRINUSE, _, _) -> Error `Already_in_use
;;

let ready t = Fiber.Ivar.read t.ready

let serve (t : t) =
  match t.state with
  | `Closed -> Code_error.raise "already closed" []
  | `Running _ -> Code_error.raise "already running" []
  | `Init fds ->
    let transport = Transport.create (List.combine t.sockaddrs fds) ~backlog:t.backlog in
    t.state <- `Running transport;
    let+ () = Fiber.Ivar.fill t.ready () in
    let loop () =
      let+ accept = Transport.accept transport in
      match accept with
      | Error _exn ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.accept
            ~success:false
            ~error:(Some "RPC accept failed. Server will not accept new clients"));
        None
      | Ok None ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.accept
            ~success:false
            ~error:(Some "No more clients will be accepted"));
        None
      | Ok (Some fd) ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.accept ~success:true ~error:None);
        let session = Session.create fd in
        Some session
    in
    Fiber.Stream.In.create loop
;;

let stop t =
  let* () = Fiber.return () in
  let+ () =
    match t.state with
    | `Closed -> Fiber.return ()
    | `Running t -> Transport.stop t
    | `Init fds ->
      List.iter ~f:Unix.close fds;
      Fiber.return ()
  in
  t.state <- `Closed
;;

let listening_address t =
  match t.state with
  | `Init fds -> List.map ~f:Unix.getsockname fds
  | `Running { Transport.sockets; _ } -> List.map ~f:fst sockets
  | `Closed -> Code_error.raise "server is already closed" []
;;
