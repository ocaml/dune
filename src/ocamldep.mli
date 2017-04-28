(** ocamldep managenemt *)

open Import

(** Generate ocamldep rules for the given modules. [item] is either the internal name of a
    library of the first name of a list of executables.

    Return arrows that evaluate to the dependency graphs.
*)
val rules
  :  Super_context.t
  -> dir:Path.t
  -> item:string
  -> modules:Module.t String_map.t
  -> alias_module:Module.t option
  -> (unit, string list String_map.t) Build.t Ml_kind.Dict.t

(** Close and convert a list of module names to a list of .cm file names *)
val names_to_top_closed_cm_files
  :  dir:Path.t
  -> dep_graph:string list String_map.t
  -> modules:Module.t String_map.t
  -> mode:Mode.t
  -> string list
  -> Path.t list
