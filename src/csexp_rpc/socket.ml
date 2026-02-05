open Stdune

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
    Exn.protectx (Unix.ADDR_UNIX sock) ~f:(f fd) ~finally:(fun _ -> pthread_chdir old)
  ;;

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
  ;;

  let connect fd ~socket = Unix.connect fd (addr socket)
  let bind fd ~socket = Unix.bind fd (addr socket)
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
let maybe_set_nosigpipe fd = if is_osx () then Mac.set_nosigpipe fd
