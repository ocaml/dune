open Stdune
open Fiber.O
open Dune_scheduler
module Session_id = Id.Make ()
module Id = Session_id

module State = struct
  type t =
    | Closed
    | Open of
        { out_buf : Io_buffer.t
        ; in_buf : Io_buffer.t
        ; fd : Unix.file_descr
        ; mutable read_eof : bool
        ; write_mutex : Fiber.Mutex.t
        ; read_mutex : Fiber.Mutex.t
        }
end

open State

type t =
  { id : Id.t
  ; mutable state : State.t
  }

let create fd =
  Unix.set_nonblock fd;
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
      }
  in
  { id; state }
;;

let close t =
  let* () = Fiber.return () in
  match t.state with
  | Closed -> Fiber.return ()
  | Open { fd; _ } ->
    t.state <- Closed;
    Async_io.close fd
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
    let rec refill () =
      if Io_buffer.length in_buf > 0
      then Fiber.return (Ok `Continue)
      else if open_.read_eof
      then Fiber.return (Ok `Eof)
      else
        let* task =
          Async_io.ready fd `Read ~f:(fun () ->
            let () = Io_buffer.maybe_resize_to_fit in_buf min_read in
            let pos = Io_buffer.write_pos in_buf in
            let len = Io_buffer.max_write_len in_buf in
            match Unix.read fd (Io_buffer.bytes in_buf) pos len with
            | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) -> `Refill
            | (exception Unix.Unix_error (ECONNRESET, _, _)) | 0 ->
              open_.read_eof <- true;
              `Eof
            | len ->
              Io_buffer.commit_write in_buf ~len;
              `Continue)
        in
        Async_io.Task.await task
        >>= function
        | Error (`Exn e) -> Fiber.return (Error e)
        | Error `Cancelled | Ok `Eof -> Fiber.return @@ Ok `Eof
        | Ok `Continue -> Fiber.return @@ Ok `Continue
        | Ok `Refill -> refill ()
    and read parser =
      let* res = refill () in
      match res with
      | Error _ as e -> Fiber.return e
      | Ok `Eof -> Fiber.return (Ok None)
      | Ok `Continue ->
        let char = Io_buffer.read_char_exn in_buf in
        let token = Lexer.feed lexer char in
        (match token with
         | Atom n ->
           Buffer.clear buf;
           atom parser n
         | (Lparen | Rparen | Await) as token ->
           let parser = Stack.add_token token parser in
           (match parser with
            | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
            | parser -> read parser))
    and atom parser n =
      if n = 0
      then (
        let atom = Buffer.contents buf in
        match Stack.add_atom atom parser with
        | Sexp (sexp, Empty) -> Fiber.return (Ok (Some sexp))
        | parser -> read parser)
      else
        refill ()
        >>= function
        | Error _ as e -> Fiber.return e
        | Ok `Eof -> Fiber.return (Ok None)
        | Ok `Continue ->
          let n' = Io_buffer.read_into_buffer in_buf buf ~max_len:n in
          atom parser (n - n')
    in
    let+ res =
      let* res = Fiber.Mutex.with_lock read_mutex ~f:(fun () -> read Stack.Empty) in
      match res with
      | Error exn ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.packet_read
            ~id:(Id.to_int t.id)
            ~success:false
            ~error:(Some (Printexc.to_string exn)));
        Dune_util.Report_error.report_exception exn;
        let+ () = close t in
        None
      | Ok None ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.packet_read ~id:(Id.to_int t.id) ~success:true ~error:None);
        let+ () = close t in
        None
      | Ok (Some sexp) ->
        Dune_trace.emit Rpc (fun () ->
          Dune_trace.Event.Rpc.packet_read ~id:(Id.to_int t.id) ~success:true ~error:None);
        Fiber.return @@ Some sexp
    in
    res
;;

external send : Unix.file_descr -> Bytes.t -> int -> int -> int = "dune_send"

let write fd bytes pos len =
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
      let* task =
        Async_io.ready fd `Write ~f:(fun () ->
          let bytes = Io_buffer.bytes out_buf in
          let pos = Io_buffer.pos out_buf in
          let len = Io_buffer.length out_buf in
          match write fd bytes pos len with
          | exception Unix.Unix_error ((EAGAIN | EINTR | EWOULDBLOCK), _, _) -> `Continue
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
       let+ () = close t in
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
       Unix.shutdown fd Unix.SHUTDOWN_ALL
     with
     | Unix.Unix_error (_, _, _) -> ());
    close t
;;
