open Stdune
open Fiber.O
open Dune_scheduler

module Transport = struct
  type t = { fd : Unix.file_descr }

  let close t = Unix.close t.fd

  let create sockaddr =
    let fd =
      Unix.socket ~cloexec:true (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
    in
    Unix.set_nonblock fd;
    { fd }
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
  let transport = Transport.create t.sockaddr in
  let fd = transport.fd in
  t.transport <- Some transport;
  Async_io.connect Socket.connect fd t.sockaddr
  >>| function
  | Ok () -> Ok (Session.create fd)
  | Error `Cancelled ->
    let exn = Failure "connect cancelled" in
    Error { Exn_with_backtrace.exn; backtrace }
  | Error (`Exn exn) -> Error { Exn_with_backtrace.exn; backtrace }
;;

let connect_exn t =
  let+ res = connect t in
  match res with
  | Ok s -> s
  | Error e -> Exn_with_backtrace.reraise e
;;

let stop t = Option.iter t.transport ~f:Transport.close
