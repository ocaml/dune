open Stdune
open Fiber.O
open Dune_scheduler

module Socket = struct
  module type Unix_socket = sig
    val connect : Fd.t -> socket:string -> unit
    val bind : Fd.t -> socket:string -> unit
  end

  module U = struct
    type sockaddr = Unix.sockaddr

    let connect fd sock = Unix.connect (Fd.unsafe_to_unix_file_descr fd) sock
    let bind fd sock = Unix.bind (Fd.unsafe_to_unix_file_descr fd) sock
  end

  module Mac = struct
    external pthread_chdir : string -> unit = "dune_pthread_chdir"
    external reset_pthread_cwd : unit -> unit = "dune_reset_pthread_cwd"
    external set_nosigpipe : Unix.file_descr -> unit = "dune_set_nosigpipe"

    let with_chdir fd ~socket ~f =
      let dir = Filename.dirname socket in
      let sock = Filename.basename socket in
      pthread_chdir dir;
      Exn.protectx (Unix.ADDR_UNIX sock) ~f:(f fd) ~finally:(fun _ ->
        reset_pthread_cwd ())
    ;;

    let connect fd ~socket : unit =
      with_chdir fd ~socket ~f:(fun fd sockaddr ->
        Unix.connect (Fd.unsafe_to_unix_file_descr fd) sockaddr)
    ;;

    let bind fd ~socket : unit =
      with_chdir fd ~socket ~f:(fun fd sockaddr ->
        Unix.bind (Fd.unsafe_to_unix_file_descr fd) sockaddr)
    ;;
  end

  module Unix : Unix_socket = struct
    let with_chdir fd ~socket ~f =
      let old = Sys.getcwd () in
      let dir = Filename.dirname socket in
      let sock = Filename.basename socket in
      Sys.chdir dir;
      Exn.protect
        ~finally:(fun () -> Sys.chdir old)
        ~f:(fun () -> f fd (Unix.ADDR_UNIX sock))
    ;;

    let connect fd ~socket = with_chdir fd ~socket ~f:U.connect
    let bind fd ~socket = with_chdir fd ~socket ~f:U.bind
  end

  module Fail : Unix_socket = struct
    let connect _ ~socket:_ = Code_error.raise "Fail.connect" []
    let bind _ ~socket:_ = Code_error.raise "Fail.bind" []
  end

  external is_osx : unit -> bool = "dune_pthread_chdir_is_osx" [@@noalloc]

  module Sel =
    (val if is_osx ()
         then (module Mac)
         else if Sys.unix
         then (module Unix)
         else (module Fail)
      : Unix_socket)

  let max_len = 104 (* 108 on some systems but we keep it conservative *)

  let make ~original ~backup fd (sa : U.sockaddr) =
    match sa with
    | ADDR_UNIX socket when String.length socket > max_len -> backup fd ~socket
    | _ -> original fd sa
  ;;

  let bind = make ~original:U.bind ~backup:Sel.bind
  let connect = make ~original:U.connect ~backup:Sel.connect

  let maybe_set_nosigpipe fd =
    if is_osx () then Mac.set_nosigpipe (Fd.unsafe_to_unix_file_descr fd)
  ;;

  let prepare_fd fd =
    match maybe_set_nosigpipe fd with
    | exception exn ->
      Fd.close fd;
      Error exn
    | () ->
      Fd.set_nonblock fd;
      Ok ()
  ;;
end

let unlink_socket_path (addr : Unix.sockaddr) =
  match addr with
  | ADDR_UNIX path -> Fpath.unlink_no_err path
  | _ -> ()
;;

let close_bound_socket_sync (addr, fd) =
  Fd.close fd;
  unlink_socket_path addr
;;

let close_bound_socket (addr, fd) =
  let+ () = Async_io.close fd in
  unlink_socket_path addr
;;

module Session = struct
  module Session_id = Id.Make ()
  module Id = Session_id

  module Fds = struct
    type t =
      | Empty
      | Range of
          { first : int
          ; count : int
          }
  end

  type fd_passing =
    { received : (int, Fd.t) Table.t
    ; mutable next_received : int
    ; mutable next_sent : int
    }

  module State = struct
    type t =
      | Closed
      | Open of
          { out_buf : Io_buffer.t
          ; in_buf : Io_buffer.t
          ; fd : Fd.t
          ; mutable read_eof : bool
          ; write_mutex : Fiber.Mutex.t
          ; read_mutex : Fiber.Mutex.t
          ; fd_passing : fd_passing option
          }
  end

  open State

  type t =
    { id : Id.t
    ; mutable state : State.t
    }

  let create fd ~fd_passing =
    let id = Id.gen () in
    Dune_trace.emit Rpc (fun () -> Dune_trace.Event.Rpc.session ~id:(Id.to_int id) `Start);
    let state =
      let size = 8192 in
      Open
        { fd
        ; in_buf = Io_buffer.create ~size
        ; out_buf = Io_buffer.create ~size
        ; read_eof = false
        ; write_mutex = Fiber.Mutex.create ()
        ; read_mutex = Fiber.Mutex.create ()
        ; fd_passing
        }
    in
    { id; state }
  ;;

  let of_fd fd =
    Socket.prepare_fd fd
    |> Result.map ~f:(fun () ->
      Fd.set_close_on_exec fd;
      let fd_passing =
        Some { received = Table.create (module Int) 16; next_received = 0; next_sent = 0 }
      in
      create fd ~fd_passing)
    |> Result.ok_exn
  ;;

  let close_fd t =
    let* () = Fiber.return () in
    match t.state with
    | Closed -> Fiber.return ()
    | Open { fd; fd_passing; _ } ->
      t.state <- Closed;
      Option.iter fd_passing ~f:(fun { received; _ } ->
        Table.iter received ~f:Fd.close;
        Table.clear received);
      Async_io.close fd
  ;;

  external send_fds_syscall
    :  Unix.file_descr
    -> Unix.file_descr array
    -> unit
    = "dune_rpc_send_fds"

  external recv_fds_syscall
    :  Unix.file_descr
    -> Bytes.t
    -> int
    -> int
    -> int * Unix.file_descr list
    = "dune_rpc_recv_fds"

  (* A NUL between packets carries SCM_RIGHTS. Both ends number the descriptors
     in stream order; requests refer to those numbers so calls can overlap. *)
  let send_fds t fds =
    match fds, t.state with
    | [], _ -> Fiber.return (Ok Fds.Empty)
    | _ :: _, Closed -> Fiber.return (Error `Closed)
    | _ :: _, Open { fd_passing = None; _ } ->
      Code_error.raise "file descriptor passing not enabled" []
    | _ :: _, Open { fd; fd_passing = Some passing; write_mutex; _ } ->
      let fds = Array.of_list (List.map fds ~f:Fd.unsafe_to_unix_file_descr) in
      let rec send () =
        match t.state with
        | Closed -> Fiber.return (Error `Closed)
        | Open _ ->
          let task =
            Async_io.ready fd `Write ~f:(fun () ->
              match send_fds_syscall (Fd.unsafe_to_unix_file_descr fd) fds with
              | () -> `Sent
              | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) -> `Retry
              | exception Unix.Unix_error ((EPIPE | ECONNRESET), _, _) -> `Closed)
          in
          Async_io.Task.await task
          >>= (function
           | Ok `Retry -> send ()
           | Ok `Sent ->
             let first = passing.next_sent in
             let count = Array.length fds in
             passing.next_sent <- first + count;
             Fiber.return (Ok (Fds.Range { first; count }))
           | Ok `Closed | Error `Cancelled -> Fiber.return (Error `Closed)
           | Error (`Exn exn) -> raise exn)
      in
      Fiber.Mutex.with_lock write_mutex ~f:send
  ;;

  let take_fds t (fds : Fds.t) =
    match fds, t.state with
    | Empty, _ -> [||]
    | Range { first; count }, Open { fd_passing = Some { received; _ }; _ } ->
      let fds = Array.init count ~f:(fun i -> Table.find_exn received (first + i)) in
      for i = first to first + count - 1 do
        Table.remove received i
      done;
      fds
    | Range _, _ -> Code_error.raise "no RPC file descriptors available" []
  ;;

  module Lexer = Csexp.Parser.Lexer
  module Stack = Csexp.Parser.Stack

  let min_read = 8192

  let read =
    fun t ->
    let* () = Fiber.return () in
    match t.state with
    | Closed ->
      Dune_trace.emit Rpc (fun () ->
        Dune_trace.Event.Rpc.packet_read ~id:(Id.to_int t.id) ~success:true ~error:None);
      Fiber.return None
    | Open ({ fd; in_buf; read_mutex; _ } as open_) ->
      let lexer = Lexer.create () in
      let buf = Buffer.create 16 in
      let eof parser =
        match parser with
        | Stack.Empty ->
          (match Lexer.feed_eoi lexer with
           | () -> Ok None
           | exception exn -> Error exn)
        | _ -> Error (Csexp.Parser.Parse_error Csexp.Parser.premature_end_of_input)
      in
      let rec refill () =
        if Io_buffer.length in_buf > 0
        then Fiber.return (Ok `Continue)
        else if open_.read_eof
        then Fiber.return (Ok `Eof)
        else (
          let read_into_buffer read =
            let () = Io_buffer.maybe_resize_to_fit in_buf min_read in
            let pos = Io_buffer.write_pos in_buf in
            let len = Io_buffer.max_write_len in_buf in
            let bytes = Io_buffer.bytes in_buf in
            match read (Fd.unsafe_to_unix_file_descr fd) bytes pos len with
            | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) -> `Refill
            | exception Unix.Unix_error (ECONNRESET, _, _) -> `Read 0
            | len -> `Read len
          in
          let* result =
            match open_.fd_passing with
            | None ->
              let task =
                Async_io.ready fd `Read ~f:(fun () -> read_into_buffer Unix.read)
              in
              Async_io.Task.await task
            | Some passing ->
              let task = Async_io.ready fd `Read ~f:ignore in
              let+ ready = Async_io.Task.await task in
              (match ready, t.state with
               | (Error _ as error), _ -> error
               | Ok (), Closed -> Error `Cancelled
               | Ok (), Open _ ->
                 (* Keep recvmsg and the CLOEXEC fallback on the scheduler
                    thread, where the worker spawns its children. *)
                 let recv fd bytes pos len =
                   let len, fds = recv_fds_syscall fd bytes pos len in
                   List.iter fds ~f:(fun fd ->
                     Table.add_exn
                       passing.received
                       passing.next_received
                       (Fd.unsafe_of_unix_file_descr fd);
                     passing.next_received <- passing.next_received + 1);
                   len
                 in
                 (match read_into_buffer recv with
                  | result -> Ok result
                  | exception exn -> Error (`Exn exn)))
          in
          match result with
          | Error (`Exn e) -> Fiber.return (Error e)
          | Error `Cancelled -> Fiber.return (Ok `Eof)
          | Ok `Refill -> refill ()
          | Ok (`Read 0) ->
            open_.read_eof <- true;
            Fiber.return (Ok `Eof)
          | Ok (`Read len) ->
            Io_buffer.commit_write in_buf ~len;
            Fiber.return (Ok `Continue))
      and read parser ~at_start =
        let* res = refill () in
        match res with
        | Error _ as e -> Fiber.return e
        | Ok `Eof -> Fiber.return (eof parser)
        | Ok `Continue ->
          let char = Io_buffer.read_char_exn in_buf in
          if at_start && Char.equal char '\000' && Option.is_some open_.fd_passing
          then read parser ~at_start
          else (
            match Lexer.feed lexer char with
            | Atom n ->
              Buffer.clear buf;
              atom parser n
            | (Lparen | Rparen | Await) as token ->
              let parser = Stack.add_token token parser in
              (match parser with
               | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
               | parser -> read parser ~at_start:false))
      and atom parser n =
        if n = 0
        then (
          let atom = Buffer.contents buf in
          match Stack.add_atom atom parser with
          | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
          | parser -> read parser ~at_start:false)
        else
          refill ()
          >>= function
          | Error _ as e -> Fiber.return e
          | Ok `Eof ->
            Fiber.return
              (Error (Csexp.Parser.Parse_error Csexp.Parser.premature_end_of_input))
          | Ok `Continue ->
            let n' = Io_buffer.read_into_buffer in_buf buf ~max_len:n in
            atom parser (n - n')
      in
      let+ res =
        let* res =
          Fiber.Mutex.with_lock read_mutex ~f:(fun () -> read Stack.Empty ~at_start:true)
        in
        match res with
        | Error exn ->
          let error = Printexc.to_string exn in
          Dune_trace.emit Rpc (fun () ->
            Dune_trace.Event.Rpc.packet_read
              ~id:(Id.to_int t.id)
              ~success:false
              ~error:(Some error));
          (match exn with
           | Csexp.Parser.Parse_error _ ->
             Log.warn
               "malformed csexp rpc packet"
               [ "id", Dyn.int (Id.to_int t.id); "error", Dyn.string error ]
           | _ -> Dune_util.Report_error.report_exception exn);
          let+ () = close_fd t in
          None
        | Ok None ->
          Dune_trace.emit Rpc (fun () ->
            Dune_trace.Event.Rpc.packet_read
              ~id:(Id.to_int t.id)
              ~success:true
              ~error:None);
          let+ () = close_fd t in
          None
        | Ok (Some sexp) ->
          Dune_trace.emit Rpc (fun () ->
            Dune_trace.Event.Rpc.packet_read
              ~id:(Id.to_int t.id)
              ~success:true
              ~error:None);
          Fiber.return @@ Some sexp
      in
      res
  ;;

  external send : Unix.file_descr -> Bytes.t -> int -> int -> int = "dune_send"

  let write fd bytes pos len =
    let fd = Fd.unsafe_to_unix_file_descr fd in
    match Platform.OS.value with
    | Linux -> send fd bytes pos len
    | _ -> Unix.single_write fd bytes pos len
  ;;

  let rec csexp_write_loop fd out_buf token =
    if Io_buffer.flushed out_buf token
    then Fiber.return (Ok ())
    else
      (* We always make sure to try and write the entire buffer.
         This should minimize the amount of [write] calls we need
         to do *)
      let* task =
        let task =
          Async_io.ready fd `Write ~f:(fun () ->
            let bytes = Io_buffer.bytes out_buf in
            let pos = Io_buffer.pos out_buf in
            let len = Io_buffer.length out_buf in
            match write fd bytes pos len with
            | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
              `Continue
            | exception Unix.Unix_error (EPIPE, _, _) -> `Cancelled
            | exception exn -> `Exn exn
            | written ->
              Io_buffer.read out_buf written;
              `Continue)
        in
        Async_io.Task.await task
      in
      match task with
      | Error _ as e -> Fiber.return e
      | Ok (`Exn exn) -> Fiber.return (Error (`Exn exn))
      | Ok `Cancelled -> Fiber.return (Error `Cancelled)
      | Ok `Continue -> csexp_write_loop fd out_buf token
  ;;

  let write t sexps =
    let* () = Fiber.return () in
    Dune_trace.emit Rpc (fun () ->
      Dune_trace.Event.Rpc.packet_write ~id:(Id.to_int t.id) ~count:(List.length sexps));
    match t.state with
    | Closed -> Fiber.return (Error `Closed)
    | Open { fd; out_buf; write_mutex; _ } ->
      let* res =
        Fiber.Mutex.with_lock write_mutex ~f:(fun () ->
          Io_buffer.write_csexps out_buf sexps;
          let flush_token = Io_buffer.flush_token out_buf in
          csexp_write_loop fd out_buf flush_token)
      in
      (match res with
       | Ok () -> Fiber.return (Ok ())
       | Error error ->
         (match error with
          | `Cancelled -> ()
          | `Exn exn ->
            Dune_trace.emit Rpc (fun () ->
              Dune_trace.Event.Rpc.dropped_write_client_disconnect exn));
         let+ () = close_fd t in
         Error `Closed)
  ;;

  let close t =
    let* () = Fiber.return () in
    Dune_trace.emit Rpc (fun () -> Dune_trace.Event.Rpc.close ~id:(Id.to_int t.id));
    match t.state with
    | Closed -> Fiber.return ()
    | Open { fd; _ } ->
      (try
         (* TODO this hack is temporary until we get rid of dune rpc init *)
         Unix.shutdown (Fd.unsafe_to_unix_file_descr fd) Unix.SHUTDOWN_ALL
       with
       | Unix.Unix_error (_, _, _) -> ());
      close_fd t
  ;;
end

module Server = struct
  module Server_id = Id.Make ()
  module Id = Server_id

  module Transport = struct
    type t =
      { sockets : (Unix.sockaddr * Fd.t) list
      ; mutable task : (Fd.t * Unix.sockaddr) Async_io.Task.t option
      ; mutable running : bool
      ; closed : unit Fiber.Ivar.t
      }

    let create sockets ~backlog =
      List.iter sockets ~f:(fun (_, fd) ->
        Unix.listen (Fd.unsafe_to_unix_file_descr fd) backlog);
      { sockets; task = None; running = true; closed = Fiber.Ivar.create () }
    ;;

    let stop t =
      let* () = Fiber.return () in
      match t.running with
      | false -> Fiber.Ivar.read t.closed
      | true ->
        t.running <- false;
        let* () =
          match t.task with
          | None -> Fiber.return ()
          | Some task ->
            t.task <- None;
            let* () = Async_io.Task.cancel task in
            let+ (_ : (Fd.t * Unix.sockaddr, [ `Cancelled | `Exn of exn ]) result) =
              Async_io.Task.await task
            in
            ()
        in
        let* () = Fiber.parallel_iter t.sockets ~f:close_bound_socket in
        Fiber.Ivar.fill t.closed ()
    ;;

    let rec accept t =
      let* () = Fiber.return () in
      match t.running with
      | false -> Fiber.return (Ok None)
      | true ->
        let task =
          Async_io.ready_one t.sockets `Read ~f:(fun _ fd ->
            let fd, sockaddr =
              Unix.accept ~cloexec:true (Fd.unsafe_to_unix_file_descr fd)
            in
            let fd = Fd.unsafe_of_unix_file_descr fd in
            let () = Result.ok_exn (Socket.prepare_fd fd) in
            fd, sockaddr)
        in
        t.task <- Some task;
        let* res = Async_io.Task.await task in
        t.task <- None;
        (match res with
         | Error (`Exn (Unix.Unix_error (Unix.EAGAIN, _, _))) -> accept t
         | Error (`Exn (Unix.Unix_error (Unix.EINVAL, _, _)))
           when Platform.OS.(value = Darwin) -> accept t
         | Error (`Exn exn) ->
           let+ () = stop t in
           Error (Exn_with_backtrace.capture exn)
         | Error `Cancelled ->
           let+ () = stop t in
           Ok None
         | Ok (fd, _) -> Fiber.return (Ok (Some fd)))
    ;;
  end

  type t =
    { id : Id.t
    ; mutable state : [ `Listening of Transport.t | `Serving of Transport.t | `Closed ]
    }

  let create sockaddrs ~backlog =
    let bound = ref [] in
    let cleanup () =
      List.iter !bound ~f:close_bound_socket_sync;
      bound := []
    in
    let bind_socket sockaddr =
      let fd =
        Unix.socket ~cloexec:true (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
        |> Fd.unsafe_of_unix_file_descr
      in
      let in_bound = ref false in
      Exn.protect
        ~finally:(fun () -> if not !in_bound then Fd.close fd)
        ~f:(fun () ->
          Fd.set_nonblock fd;
          (match (sockaddr : Unix.sockaddr) with
           | ADDR_UNIX _ -> ()
           | ADDR_INET _ ->
             Unix.setsockopt (Fd.unsafe_to_unix_file_descr fd) Unix.SO_REUSEADDR true);
          Socket.bind fd sockaddr;
          bound := (sockaddr, fd) :: !bound;
          in_bound := true;
          fd)
    in
    try
      Exn.protect ~finally:cleanup ~f:(fun () ->
        let fds = List.map sockaddrs ~f:bind_socket in
        let transport = Transport.create (List.combine sockaddrs fds) ~backlog in
        bound := [];
        Ok { id = Id.gen (); state = `Listening transport })
    with
    | Unix.Unix_error (EADDRINUSE, _, _) -> Error `Already_in_use
  ;;

  let serve (t : t) =
    let transport =
      match t.state with
      | `Closed -> Code_error.raise "already closed" []
      | `Serving _ -> Code_error.raise "already running" []
      | `Listening transport -> transport
    in
    t.state <- `Serving transport;
    let loop () =
      Dune_trace.emit Rpc (fun () ->
        Dune_trace.Event.Rpc.accept ~id:(Id.to_int t.id) `Start None);
      let+ accept = Transport.accept transport in
      Dune_trace.emit Rpc (fun () ->
        let res =
          match accept with
          | Error exn -> `Error exn
          | Ok None -> `Close
          | Ok (Some _) -> `Accept
        in
        Dune_trace.Event.Rpc.accept ~id:(Id.to_int t.id) `Stop (Some res));
      match accept with
      | Error _ | Ok None -> None
      | Ok (Some fd) -> Some (Session.create fd ~fd_passing:None)
    in
    Fiber.return (Fiber.Stream.In.create loop)
  ;;

  let stop t =
    let* () = Fiber.return () in
    match t.state with
    | `Closed -> Fiber.return ()
    | `Listening _ | `Serving _ ->
      Dune_trace.emit Rpc (fun () ->
        Dune_trace.Event.Rpc.shutdown ~id:(Id.to_int t.id) `Start);
      let* () =
        match t.state with
        | `Closed -> Fiber.return ()
        | `Listening transport | `Serving transport -> Transport.stop transport
      in
      let+ () = Fiber.return () in
      t.state <- `Closed;
      Dune_trace.emit Rpc (fun () ->
        Dune_trace.Event.Rpc.shutdown ~id:(Id.to_int t.id) `Stop)
  ;;

  let socket_name ((addr, fd) : Unix.sockaddr * Fd.t) =
    match addr with
    | ADDR_UNIX _ -> addr
    | ADDR_INET _ -> Unix.getsockname (Fd.unsafe_to_unix_file_descr fd)
  ;;

  let listening_address t =
    match t.state with
    | `Listening { Transport.sockets; _ } | `Serving { Transport.sockets; _ } ->
      List.map ~f:socket_name sockets
    | `Closed -> Code_error.raise "server is already closed" []
  ;;
end

module Client = struct
  module Transport = struct
    type t = { fd : Fd.t }

    let close t = Fd.close t.fd

    let create sockaddr =
      let fd =
        Unix.socket ~cloexec:true (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
        |> Fd.unsafe_of_unix_file_descr
      in
      match Socket.prepare_fd fd with
      | Ok () -> Ok { fd }
      | Error exn -> Error exn
    ;;
  end

  type t =
    { mutable transport : Transport.t option
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr = { sockaddr; transport = None }

  let connect t =
    let* () = Fiber.return () in
    let backtrace = Printexc.get_callstack 10 in
    match Transport.create t.sockaddr with
    | Error exn -> Fiber.return (Error { Exn_with_backtrace.exn; backtrace })
    | Ok transport ->
      let fd = transport.fd in
      t.transport <- Some transport;
      Async_io.connect Socket.connect fd t.sockaddr
      >>| (function
       | Ok () ->
         t.transport <- None;
         Ok (Session.create fd ~fd_passing:None)
       | Error `Cancelled ->
         Transport.close transport;
         t.transport <- None;
         let exn = Failure "connect cancelled" in
         Error { Exn_with_backtrace.exn; backtrace }
       | Error (`Exn exn) ->
         Transport.close transport;
         t.transport <- None;
         Error { Exn_with_backtrace.exn; backtrace })
  ;;

  let connect_exn t =
    connect t
    >>| function
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e
  ;;

  let stop t =
    match t.transport with
    | None -> ()
    | Some transport ->
      t.transport <- None;
      Transport.close transport
  ;;
end
