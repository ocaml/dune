open Stdune

type t

val make
  :  ctx:Context.t
  -> installable_modules:Module.t list
  -> dir:Path.t
  -> Dune_file.Library.t
  -> t

val files : t -> Path.t list
val dlls : t -> Path.t list

val all : t -> Path.Set.t
