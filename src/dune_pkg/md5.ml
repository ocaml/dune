open Stdune

type t = string

external md5_fd : Unix.file_descr -> string = "dune_pkg_md5_fd"

module D = Stdlib.Digest

module type Digest_impl = sig
  val file : string -> t
  val string : string -> t
end

module Direct_impl : Digest_impl = struct
  let file file =
    (* On Windows, if this function is invoked in a background thread,
       if can happen that the file is not properly closed.
       [O_SHARE_DELETE] ensures that the main thread can delete it even if it
       is still open. See #8243. *)
    let fd =
      match Unix.openfile file [ Unix.O_RDONLY; O_SHARE_DELETE; O_CLOEXEC ] 0 with
      | fd -> fd
      | exception Unix.Unix_error (Unix.EACCES, _, _) ->
        raise (Sys_error (sprintf "%s: Permission denied" file))
      | exception exn -> reraise exn
    in
    Exn.protectx fd ~f:md5_fd ~finally:Unix.close
  ;;

  let string = D.string
end

module Mutable_impl = struct
  let file_ref = ref Direct_impl.file
  let string_ref = ref D.string
  let file f = !file_ref f
  let string s = !string_ref s
end

module Impl : Digest_impl = Mutable_impl

let equal = String.equal
let file p = Impl.file (Path.to_string p)
let to_string = D.to_hex
let to_dyn s = Dyn.variant "digest" [ String (to_string s) ]

let from_hex s =
  match D.from_hex s with
  | s -> Some s
  | exception Invalid_argument _ -> None
;;

let string = Impl.string
