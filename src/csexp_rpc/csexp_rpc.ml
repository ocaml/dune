open Stdune
open Fiber.O
module Log = Dune_util.Log

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

module Socket = struct
  module type Unix_socket = sig
    val connect : Unix.file_descr -> socket:string -> unit

    val bind : Unix.file_descr -> socket:string -> unit
  end

  module U = struct
    type sockaddr = Unix.sockaddr

    let connect fd sock = Unix.connect fd sock

    let bind fd sock = Unix.bind fd sock
  end

  module Mac : Unix_socket = struct
    external pthread_chdir : string -> unit = "dune_pthread_chdir" [@@noalloc]

    let with_chdir fd ~socket ~f =
      let old = Sys.getcwd () in
      let dir = Filename.dirname socket in
      let sock = Filename.basename socket in
      pthread_chdir dir;
      Exn.protectx (Unix.ADDR_UNIX sock) ~f:(f fd) ~finally:(fun _ ->
          pthread_chdir old)

    let connect fd ~socket : unit = with_chdir fd ~socket ~f:Unix.connect

    let bind fd ~socket : unit = with_chdir fd ~socket ~f:Unix.bind
  end

  module Unix : Unix_socket = struct
    let addr socket =
      Unix.ADDR_UNIX
        (match
           let cwd = Sys.getcwd () in
           String.drop_prefix socket ~prefix:(cwd ^ "/")
         with
        | Some s -> "./" ^ s
        | None -> socket)

    let connect fd ~socket = Unix.connect fd (addr socket)

    let bind fd ~socket = Unix.bind fd (addr socket)
  end

  module Fail : Unix_socket = struct
    let connect _ ~socket:_ = Code_error.raise "Fail.connect" []

    let bind _ ~socket:_ = Code_error.raise "Fail.bind" []
  end

  external is_osx : unit -> bool = "dune_pthread_chdir_is_osx" [@@noalloc]

  module Sel = (val if is_osx () then (module Mac)
                    else if Sys.unix then (module Unix)
                    else (module Fail) : Unix_socket)

  let max_len = 104 (* 108 on some systems but we keep it conservative *)

  let make ~original ~backup fd (sa : U.sockaddr) =
    match sa with
    | ADDR_UNIX socket when String.length socket > max_len -> backup fd ~socket
    | _ -> original fd sa

  let bind = make ~original:U.bind ~backup:Sel.bind

  let connect = make ~original:U.connect ~backup:Sel.connect
end

let debug = Option.is_some (Env.get Env.initial "DUNE_RPC_DEBUG")

module Session = struct
  module Id = Session_id

  type state =
    | Closed
    | Open of
        { out_channel : out_channel
        ; in_channel : in_channel
        ; socket : bool
        ; writer : Worker.t
        ; reader : Worker.t
        }

  type t =
    { id : Id.t
    ; mutable state : state
    }

  let create ~socket in_channel out_channel =
    let id = Id.gen () in
    if debug then
      Log.info [ Pp.textf "RPC created new session %d" (Id.to_int id) ];
    let* reader = Worker.create () in
    let+ writer = Worker.create () in
    let state = Open { in_channel; out_channel; reader; writer; socket } in
    { id; state }

  let string_of_packet = function
    | None -> "EOF"
    | Some csexp -> Sexp.to_string csexp

  let string_of_packets = function
    | None -> "EOF"
    | Some sexps -> String.concat ~sep:" " (List.map ~f:Sexp.to_string sexps)

  let close t =
    match t.state with
    | Closed -> ()
    | Open { in_channel; out_channel; reader; writer; socket } ->
      Worker.stop reader;
      Worker.stop writer;
      (* with a socket, there's only one fd. We make sure to close it only once.
         with dune rpc init, we have two separate fd's (stdin/stdout) so we must
         close both. *)
      if not socket then close_in_noerr in_channel;
      close_out_noerr out_channel;
      t.state <- Closed

  let read t =
    let debug res =
      if debug then
        Log.info
          [ Pp.verbatim
              (sprintf "RPC (%d) <<\n%s" (Id.to_int t.id) (string_of_packet res))
          ; Pp.text "<<"
          ]
    in
    match t.state with
    | Closed ->
      debug None;
      Fiber.return None
    | Open { reader; in_channel; _ } ->
      let rec read () =
        match Csexp.input_opt in_channel with
        | exception Unix.Unix_error (_, _, _) -> None
        | exception Sys_error _ -> None
        | exception Sys_blocked_io -> read ()
        | Ok None -> None
        | Ok (Some csexp) -> Some csexp
        | Error _ -> None
      in
      let+ res = Worker.task reader ~f:read in
      let res =
        match res with
        | Error (`Exn _) ->
          close t;
          None
        | Error `Stopped -> None
        | Ok None ->
          close t;
          None
        | Ok (Some sexp) -> Some sexp
      in
      debug res;
      res

  let write t sexps =
    if debug then
      Log.info
        [ Pp.verbatim
            (sprintf "RPC (%id) >>\n%s" (Id.to_int t.id)
               (string_of_packets sexps))
        ; Pp.text ">>"
        ];
    match t.state with
    | Closed -> (
      match sexps with
      | None -> Fiber.return ()
      | Some sexps ->
        Code_error.raise "attempting to write to a closed channel"
          [ ("sexp", Dyn.(list Sexp.to_dyn) sexps) ])
    | Open { writer; out_channel; socket; _ } -> (
      match sexps with
      | None ->
        (if socket then
         try
           (* TODO this hack is temporary until we get rid of dune rpc init *)
           Unix.shutdown
             (Unix.descr_of_out_channel out_channel)
             Unix.SHUTDOWN_ALL
         with Unix.Unix_error (_, _, _) -> ());
        close t;
        Fiber.return ()
      | Some sexps -> (
        let+ res =
          Worker.task writer ~f:(fun () ->
              List.iter sexps ~f:(Csexp.to_channel out_channel);
              flush out_channel)
        in
        match res with
        | Ok () -> ()
        | Error `Stopped -> assert false
        | Error (`Exn e) ->
          close t;
          Exn_with_backtrace.reraise e))
end

let close_fd_no_error fd = try Unix.close fd with _ -> ()

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      ; r_interrupt_accept : Unix.file_descr
      ; w_interrupt_accept : Unix.file_descr
      ; buf : Bytes.t
      }

    let create fd sockaddr ~backlog =
      Unix.listen fd backlog;
      let r_interrupt_accept, w_interrupt_accept = Unix.pipe ~cloexec:true () in
      Unix.set_nonblock r_interrupt_accept;
      let buf = Bytes.make 1 '0' in
      { fd; sockaddr; r_interrupt_accept; w_interrupt_accept; buf }

    let rec accept t =
      match Unix.select [ t.r_interrupt_accept; t.fd ] [] [] (-1.0) with
      | r, [], [] ->
        let inter, accept =
          List.fold_left r ~init:(false, false) ~f:(fun (i, a) fd ->
              if fd = t.fd then (i, true)
              else if fd = t.r_interrupt_accept then (true, a)
              else assert false)
        in
        if inter then None
        else if accept then (
          let fd, _ = Unix.accept ~cloexec:true t.fd in
          Unix.clear_nonblock fd;
          Some fd)
        else assert false
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
    { mutable state :
        [ `Init of Unix.file_descr | `Running of Transport.t | `Closed ]
    ; backlog : int
    ; sockaddr : Unix.sockaddr
    ; ready : unit Fiber.Ivar.t
    }

  let create sockaddr ~backlog =
    let fd =
      Unix.socket ~cloexec:true
        (Unix.domain_of_sockaddr sockaddr)
        Unix.SOCK_STREAM 0
    in
    Unix.set_nonblock fd;
    Unix.setsockopt fd Unix.SO_REUSEADDR true;
    match Socket.bind fd sockaddr with
    | exception Unix.Unix_error (EADDRINUSE, _, _) -> Error `Already_in_use
    | () ->
      Ok { sockaddr; backlog; state = `Init fd; ready = Fiber.Ivar.create () }

  let ready t = Fiber.Ivar.read t.ready

  let serve (t : t) =
    let* async = Worker.create () in
    match t.state with
    | `Closed -> Code_error.raise "already closed" []
    | `Running _ -> Code_error.raise "already running" []
    | `Init fd ->
      let* transport =
        Worker.task_exn async ~f:(fun () ->
            Transport.create fd t.sockaddr ~backlog:t.backlog)
      in
      t.state <- `Running transport;
      let+ () = Fiber.Ivar.fill t.ready () in
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
        | Error `Stopped ->
          Log.info [ Pp.text "RPC stopped accepting." ];
          Fiber.return None
        | Error (`Exn exn) ->
          Log.info
            [ Pp.text "RPC accept failed. Server will not accept new clients"
            ; Exn_with_backtrace.pp exn
            ];
          Fiber.return None
        | Ok None ->
          Log.info
            [ Pp.text
                "RPC accepted the last client. No more clients will be \
                 accepted."
            ];
          Fiber.return None
        | Ok (Some (in_, out)) ->
          let+ session = Session.create ~socket:true in_ out in
          Some session
      in
      Fiber.Stream.In.create loop

  let stop t =
    let () =
      match t.state with
      | `Closed -> ()
      | `Running t -> Transport.stop t
      | `Init fd -> Unix.close fd
    in
    t.state <- `Closed

  let listening_address t =
    match t.state with
    | `Init fd | `Running { Transport.fd; _ } -> Unix.getsockname fd
    | `Closed -> Code_error.raise "server is already closed" []
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
      let () = Socket.connect t.fd t.sockaddr in
      t.fd
  end

  type t =
    { mutable transport : Transport.t option
    ; mutable async : Worker.t option
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr =
    let+ async = Worker.create () in
    { sockaddr; async = Some async; transport = None }

  let connect t =
    match t.async with
    | None ->
      Code_error.raise "connection already established with the client" []
    | Some async -> (
      t.async <- None;
      let* task =
        Worker.task async ~f:(fun () ->
            let transport = Transport.create t.sockaddr in
            t.transport <- Some transport;
            let client = Transport.connect transport in
            let out = Unix.out_channel_of_descr client in
            let in_ = Unix.in_channel_of_descr client in
            (in_, out))
      in
      Worker.stop async;
      match task with
      | Error `Stopped -> assert false
      | Error (`Exn exn) -> Fiber.return (Error exn)
      | Ok (in_, out) ->
        let+ res = Session.create ~socket:true in_ out in
        Ok res)

  let connect_exn t =
    let+ res = connect t in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e

  let stop t = Option.iter t.transport ~f:Transport.close
end
