open Stdune

module Group : sig
  type t =
    | Cmi
    | Cmx
    | Header
end

(** [file_deps t libs ~files] returns a list of path dependencies for all the
    files with extension [files] of libraries [libs]. *)
val file_deps
  :  Lib.L.t
  -> groups:Group.t list
  -> Path.t list

val file_deps_with_exts
  :  (Lib.t * Group.t list) list
  -> Path.t list
