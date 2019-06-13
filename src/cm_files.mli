(** This module encapsulates the trick of speeding up builds by providing an
    unsorted list of module dependencies statically and only using the topsorted
    list of deps for the order when passing to ocamlopt *)

open Stdune

type t

val make_exe
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Module.Name_map.t
  -> top_sorted_modules:(unit, Module.t list) Build.t
  -> ext_obj:string
  -> t

val make_lib
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Module.t list
  -> top_sorted_modules:(unit, Module.t list) Build.t
  -> ext_obj:string
  -> t

val unsorted_objects_and_cms : t -> mode:Mode.t -> Path.t list

val top_sorted_cms : t -> mode:Mode.t -> (unit, Path.t list) Build.t
