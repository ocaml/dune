open Stdune

type t = string

let equal = String.equal
let to_hex = Stdlib.Digest.to_hex
let to_dyn s = Dyn.variant "digest" [ String (to_hex s) ]

let of_hex s =
  match Stdlib.Digest.from_hex s with
  | s -> Some s
  | exception Invalid_argument _ -> None
;;

external md5_fd : Unix.file_descr -> string = "dune_pkg_md5_fd"

let file file =
  (* On Windows, if this function is invoked in a background thread, if can happen that
     the file is not properly closed. [O_SHARE_DELETE] ensures that the main thread can
     delete it even if it is still open. See #8243. *)
  let file = Path.to_string file in
  Result.try_with (fun () ->
    let fd = Unix.openfile file [ Unix.O_RDONLY; O_SHARE_DELETE; O_CLOEXEC ] 0 in
    Exn.protectx fd ~f:md5_fd ~finally:Unix.close)
;;

let string = Stdlib.Digest.string
