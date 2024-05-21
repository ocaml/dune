(** This module encapsulates the trick of speeding up builds by providing an
    unsorted list of module dependencies statically and only using the topsorted
    list of deps for the order when passing to ocamlopt *)

open Import

type t

val make
  :  ?excluded_modules:Module_name.t list
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> top_sorted_modules:Module.t list Action_builder.t
  -> ext_obj:Filename.Extension.t
  -> unit
  -> t

val unsorted_objects_and_cms : t -> mode:Mode.t -> Path.t list
val top_sorted_cms : t -> mode:Mode.t -> Path.t list Action_builder.t
val top_sorted_objects_and_cms : t -> mode:Mode.t -> Path.t list Action_builder.t
