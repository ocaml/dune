open Import

(** Archive names appear as part of the [(foreign_archives ...)] fields, as well as in the
    [(archive_name ...)] field of the [(foreign_library ...)] stanza. For example, both
    [(foreign_archives some/dir/lib)] and [(archive_name lib)] specify the same archive
    name [lib]. Archive names are not allowed to contain path separators. *)
module Name : sig
  type t

  module Map : Map.S with type key = t

  val to_dyn : t -> Dyn.t
  val to_string : t -> string
  val path : dir:Path.Build.t -> mode:Mode.Select.t -> t -> Path.Build.t
  val decode : t Dune_lang.Decoder.t
  val stubs : string -> t
  val lib_file_prefix : string

  val lib_file
    :  t
    -> dir:Path.Build.t
    -> ext_lib:Filename.Extension.t
    -> mode:Mode.Select.t
    -> Path.Build.t

  val dll_file
    :  t
    -> dir:Path.Build.t
    -> ext_dll:Filename.Extension.t
    -> mode:Mode.Select.t
    -> Path.Build.t
end

(** Foreign archives appear in the [(foreign_archives ...)] field of libraries and
    executables, for example [(foreign_archives some/dir/lib)]. When parsing such fields,
    we separate the directory [some/dir] from the name [lib] of the archive and store them
    as a [{dir : Dir.t; name : Name.t}] record. *)
type t

val dir_path : dir:Path.Build.t -> t -> Path.Build.t
val name : mode:Mode.Select.t -> t -> Name.t
val stubs : string -> t
val decode : t Dune_lang.Decoder.t

val lib_file
  :  archive:t
  -> dir:Path.Build.t
  -> ext_lib:Filename.Extension.t
  -> mode:Mode.Select.t
  -> Path.Build.t

val dll_file
  :  archive:t
  -> dir:Path.Build.t
  -> ext_dll:Filename.Extension.t
  -> mode:Mode.Select.t
  -> Path.Build.t
