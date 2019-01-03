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
  :  Super_context.t
  -> Lib.L.t
  -> groups:Group.t list
  -> Path.t list

val file_deps_with_exts
  :  Super_context.t
  -> (Lib.t * Group.t list) list
  -> Path.t list

(** Setup alias dependencies for library artifacts grouped by extensions *)
val setup_file_deps
  :  Super_context.t
  -> dir:Path.t
  -> lib:Dune_file.Library.t
  -> modules:Module.t list
  -> unit
