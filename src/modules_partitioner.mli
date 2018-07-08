(** Checks modules partitioning inside a directory *)

open! Stdune

type 'a t

val create : unit -> 'a t

(** [acknowledge t partition ~loc ~modules] registers the fact that [modules]
    are associated with [loc]. *)
val acknowledge
  :  'a t
  -> 'a
  -> loc:Loc.t
  -> modules:Module.t Module.Name.Map.t
  -> dune_version:Syntax.Version.t
  -> unit

(** Find which partition a module is part of *)
val find : 'a t -> Module.Name.t -> 'a option

(** To be called after processing a directory. Emit errors or warnings
    about detected problems *)
val emit_errors : _ t -> unit
