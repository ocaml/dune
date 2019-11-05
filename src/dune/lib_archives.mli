open Stdune

type t

val has_native_archive :
  Dune_file.Library.t -> Lib_config.t -> Dir_contents.t -> bool

val make :
     ctx:Context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Dune_file.Library.t
  -> t

val lib_files : t -> Path.Build.t list

val dll_files : t -> Path.Build.t list
