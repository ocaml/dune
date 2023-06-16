open Stdune
open Fiber.O
open Dune_async_io
module Log = Dune_util.Log

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

  module Mac = struct
    external pthread_chdir : string -> unit = "dune_pthread_chdir" [@@noalloc]

    external set_nosigpipe : Unix.file_descr -> unit = "dune_set_nosigpipe"

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

  let maybe_set_nosigpipe fd = if is_osx () then Mac.set_nosigpipe fd
end

let debug = Option.is_some (Env.get Env.initial "DUNE_RPC_DEBUG")

module Session = struct
  let fail = function
    | `Cancelled -> raise Dune_util.Report_error.Already_reported
    | `Exn exn -> raise exn

  module Id = Session_id

  type state =
    | Closed
    | Open of
        { out_buf : Io_buffer.t
        ; in_buf : Io_buffer.t
        ; fd : Unix.file_descr
        ; mutable read_eof : bool
        ; write_mutex : Fiber.Mutex.t
        ; read_mutex : Fiber.Mutex.t
        }

  type t =
    { id : Id.t
    ; mutable state : state
    }

  let create fd =
    Unix.set_nonblock fd;
    let id = Id.gen () in
    if debug then
      Log.info [ Pp.textf "RPC created new session %d" (Id.to_int id) ];
    let state =
      let size = 8192 in
      Open
        { fd
        ; in_buf = Io_buffer.create ~size
        ; out_buf = Io_buffer.create ~size
        ; read_eof = false
        ; write_mutex = Fiber.Mutex.create ()
        ; read_mutex = Fiber.Mutex.create ()
        }
    in
    { id; state }

  let string_of_packet = function
    | None -> "EOF"
    | Some csexp -> Dyn.to_string (Sexp.to_dyn csexp)

  let string_of_packets = function
    | None -> "EOF"
    | Some sexps -> String.concat ~sep:" " (List.map ~f:Sexp.to_string sexps)

  let close t =
    let* () = Fiber.return () in
    match t.state with
    | Closed -> Fiber.return ()
    | Open { fd; _ } ->
      let+ () = Async_io.close fd in
      t.state <- Closed

  module Lexer = Csexp.Parser.Lexer
  module Stack = Csexp.Parser.Stack

  let min_read = 8192

  let read t =
    let* () = Fiber.return () in
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
    | Open ({ fd; in_buf; read_mutex; _ } as open_) ->
      let lexer = Lexer.create () in
      let buf = Buffer.create 16 in
      let rec refill () =
        if Io_buffer.length in_buf > 0 then Fiber.return (Ok `Continue)
        else if open_.read_eof then Fiber.return (Ok `Eof)
        else
          let* task =
            Async_io.ready fd `Read ~f:(fun () ->
                let () = Io_buffer.maybe_resize_to_fit in_buf min_read in
                let pos = Io_buffer.write_pos in_buf in
                let len = Io_buffer.max_write_len in_buf in
                match Unix.read fd (Io_buffer.bytes in_buf) pos len with
                | exception
                    Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) ->
                  `Refill
                | 0 ->
                  open_.read_eof <- true;
                  `Eof
                | len ->
                  Io_buffer.commit_write in_buf ~len;
                  `Continue)
          in
          Async_io.Task.await task >>= function
          | Error (`Exn e) -> Fiber.return (Error e)
          | Error `Cancelled | Ok `Eof -> Fiber.return @@ Ok `Eof
          | Ok `Continue -> Fiber.return @@ Ok `Continue
          | Ok `Refill -> refill ()
      and read parser =
        let* res = refill () in
        match res with
        | Error _ as e -> Fiber.return e
        | Ok `Eof -> Fiber.return (Ok None)
        | Ok `Continue -> (
          let char = Io_buffer.read_char_exn in_buf in
          let token = Lexer.feed lexer char in
          match token with
          | Atom n ->
            Buffer.clear buf;
            atom parser n
          | (Lparen | Rparen | Await) as token -> (
            let parser = Stack.add_token token parser in
            match parser with
            | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
            | parser -> read parser))
      and atom parser n =
        if n = 0 then
          let atom = Buffer.contents buf in
          match Stack.add_atom atom parser with
          | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
          | parser -> read parser
        else
          refill () >>= function
          | Error _ as e -> Fiber.return e
          | Ok `Eof -> Fiber.return (Ok None)
          | Ok `Continue ->
            let n' = Io_buffer.read_into_buffer in_buf buf ~max_len:n in
            atom parser (n - n')
      in
      let+ res =
        let* res =
          Fiber.Mutex.with_lock read_mutex ~f:(fun () -> read Stack.Empty)
        in
        match res with
        | Error exn ->
          Log.info
            [ Pp.textf "Unable to read (%d)" (Id.to_int t.id); Exn.pp exn ];
          let+ () = close t in
          reraise exn
        | Ok None ->
          let+ () = close t in
          None
        | Ok (Some sexp) -> Fiber.return @@ Some sexp
      in
      debug res;
      res

  external send : Unix.file_descr -> Bytes.t -> int -> int -> int = "dune_send"

  let write =
    match Platform.OS.value with
    | Linux -> send
    | _ -> Unix.single_write

  let rec csexp_write_loop fd out_buf token =
    if Io_buffer.flushed out_buf token then Fiber.return (Ok ())
    else
      (* We always make sure to try and write the entire buffer.
          This should minimize the amount of [write] calls we need
          to do *)
      let* task =
        let* task =
          Async_io.ready fd `Write ~f:(fun () ->
              let bytes = Io_buffer.bytes out_buf in
              let pos = Io_buffer.pos out_buf in
              let len = Io_buffer.length out_buf in
              match write fd bytes pos len with
              | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _)
                -> `Continue
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

  let write t sexps =
    let* () = Fiber.return () in
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
    | Open { fd; out_buf; write_mutex; _ } -> (
      match sexps with
      | None ->
        (try
           (* TODO this hack is temporary until we get rid of dune rpc init *)
           Unix.shutdown fd Unix.SHUTDOWN_ALL
         with Unix.Unix_error (_, _, _) -> ());
        close t
      | Some sexps -> (
        let* res =
          Fiber.Mutex.with_lock write_mutex ~f:(fun () ->
              Io_buffer.write_csexps out_buf sexps;
              let flush_token = Io_buffer.flush_token out_buf in
              csexp_write_loop fd out_buf flush_token)
        in
        match res with
        | Ok () -> Fiber.return ()
        | Error error ->
          let+ () = close t in
          fail error))
end

module Server = struct
  module Transport = struct
    type t =
      { fd : Unix.file_descr
      ; sockaddr : Unix.sockaddr
      ; mutable task : (Unix.file_descr * Unix.sockaddr) Async_io.Task.t option
      ; mutable running : bool
      }

    let create fd sockaddr ~backlog =
      Unix.listen fd backlog;
      Unix.set_nonblock fd;
      { fd; sockaddr; task = None; running = true }

    let close t =
      let+ () = Async_io.close t.fd in
      Ok None

    let rec accept t =
      let* () = Fiber.return () in
      match t.running with
      | false -> close t
      | true -> (
        let* task =
          Async_io.ready t.fd `Read ~f:(fun () ->
              Unix.accept ~cloexec:true t.fd)
        in
        t.task <- Some task;
        let* res = Async_io.Task.await task in
        match res with
        | Error (`Exn (Unix.Unix_error (Unix.EAGAIN, _, _))) -> accept t
        | Error (`Exn exn) ->
          let+ _ = close t in
          Error (Exn_with_backtrace.capture exn)
        | Error `Cancelled -> close t
        | Ok (fd, _) ->
          Socket.maybe_set_nosigpipe fd;
          Unix.set_nonblock fd;
          Fiber.return @@ Ok (Some fd))

    let stop t =
      let* () = Fiber.return () in
      t.running <- false;
      let+ () =
        match t.task with
        | None -> Fiber.return ()
        | Some task -> Async_io.Task.cancel task
      in
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
    match t.state with
    | `Closed -> Code_error.raise "already closed" []
    | `Running _ -> Code_error.raise "already running" []
    | `Init fd ->
      let transport = Transport.create fd t.sockaddr ~backlog:t.backlog in
      t.state <- `Running transport;
      let+ () = Fiber.Ivar.fill t.ready () in
      let loop () =
        let+ accept = Transport.accept transport in
        match accept with
        | Error exn ->
          Log.info
            [ Pp.text "RPC accept failed. Server will not accept new clients"
            ; Exn_with_backtrace.pp exn
            ];
          None
        | Ok None ->
          Log.info
            [ Pp.text
                "RPC accepted the last client. No more clients will be \
                 accepted."
            ];
          None
        | Ok (Some fd) ->
          let session = Session.create fd in
          Some session
      in
      Fiber.Stream.In.create loop

  let stop t =
    let* () = Fiber.return () in
    let+ () =
      match t.state with
      | `Closed -> Fiber.return ()
      | `Running t -> Transport.stop t
      | `Init fd ->
        Unix.close fd;
        Fiber.return ()
    in
    t.state <- `Closed

  let listening_address t =
    match t.state with
    | `Init fd | `Running { Transport.fd; _ } -> Unix.getsockname fd
    | `Closed -> Code_error.raise "server is already closed" []
end

module Client = struct
  module Transport = struct
    type t = { fd : Unix.file_descr }

    let close t = Unix.close t.fd

    let create sockaddr =
      let fd =
        Unix.socket ~cloexec:true
          (Unix.domain_of_sockaddr sockaddr)
          Unix.SOCK_STREAM 0
      in
      Unix.set_nonblock fd;
      { fd }
  end

  type t =
    { mutable transport : Transport.t option
    ; sockaddr : Unix.sockaddr
    }

  let create sockaddr = { sockaddr; transport = None }

  let connect t =
    let* () = Fiber.return () in
    let backtrace = Printexc.get_callstack 10 in
    let transport = Transport.create t.sockaddr in
    let fd = transport.fd in
    t.transport <- Some transport;
    Async_io.connect Socket.connect fd t.sockaddr >>| function
    | Ok () -> Ok (Session.create fd)
    | Error `Cancelled ->
      let exn = Failure "connect cancelled" in
      Error { Exn_with_backtrace.exn; backtrace }
    | Error (`Exn exn) -> Error { Exn_with_backtrace.exn; backtrace }

  let connect_exn t =
    let+ res = connect t in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e

  let stop t = Option.iter t.transport ~f:Transport.close
end

module Private = struct
  module Io_buffer = Io_buffer
end
