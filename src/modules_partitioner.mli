(** Checks modules partitioning inside a directory *)

type t

val create
  :  dir:Path.t
  -> all_modules:Module.t Module.Name.Map.t
  -> t

(** [acknowledge t ~loc ~modules] registers the fact that [modules]
    are associated with [loc].

    Returns the set of modules that are already used at another
    location.
*)
val acknowledge
  :  t
  -> loc:Loc.t
  -> modules:Module.t Module.Name.Map.t
  -> Module.Name.Set.t

(** To be called after processing a directory. Emit warnings about
    detected problems *)
val emit_warnings : t -> unit
