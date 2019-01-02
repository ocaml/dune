open Stdune

module L : sig

  (** [file_deps t libs ~ext] returns a list of path dependencies for all the
      files with extension [ext] of libraries [libs]. *)
  val file_deps : Super_context.t -> Lib.L.t -> exts:string list -> Path.t list

  val file_deps_with_exts
    :  Super_context.t
    -> (Lib.t * string list) list
    -> Path.t list
end

(** Setup alias dependencies for library artifacts grouped by extensions *)
val setup_file_deps
  :  Super_context.t
  -> dir:Path.t
  -> lib:Dune_file.Library.t
  -> modules:Module.t list
  -> unit
