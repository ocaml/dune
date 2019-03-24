open Stdune

module Group : sig
  type t =
    | Cmi
    | Cmx
    | Header
end

(** [file_deps t libs ~files] returns a list of path dependencies for all the
    files with extension [files] of libraries [libs]. *)
val deps
  :  Lib.L.t
  -> groups:Group.t list
  -> Dep.Set.t

val deps_with_exts
  :  (Lib.t * Group.t list) list
  -> Dep.Set.t

(** Setup alias dependencies for library artifacts grouped by extensions *)
val setup_file_deps
  :  dir:Path.t
  -> lib:Dune_file.Library.t
  -> modules:Module.t list
  -> unit
