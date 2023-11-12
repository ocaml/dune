(** Action builder *)

open! Import
include module type of Action_builder0

(** Record the given set as dependencies of the action produced by the action builder. *)
val record : Dep.Set.t -> unit t
