(** Checks modules partitioning inside a directory *)

open Import

type t

val create
  :  dir:Path.t
  -> all_modules:Module.t String_map.t
  -> t

(** [acknowledge t ~loc ~modules] registers the fact that [modules]
    are associated with [loc].

    Returns the set of modules that are already used at another
    location.
*)
val acknowledge
  :  t
  -> loc:Loc.t
  -> modules:Module.t String_map.t
  -> String_set.t

(** To be called after processing a directory. Emit warnings about
    detected problems *)
val emit_warnings : t -> unit
