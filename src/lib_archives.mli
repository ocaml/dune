open Stdune

type t =
  { dlls : Path.t list
  ; files : Path.t list
  }

val make
  :  ctx:Context.t
  -> installable_modules:Module.t list
  -> dir:Path.t
  -> Dune_file.Library.t
  -> t
