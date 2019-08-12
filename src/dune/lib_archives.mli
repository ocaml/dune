open Stdune

type t

val make :
     ctx:Context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Dune_file.Library.t
  -> t

val files : t -> Path.Build.t list

val dlls : t -> Path.Build.t list
