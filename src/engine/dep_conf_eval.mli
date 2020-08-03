(** Interpret dependencies written in Dune files *)
open Stdune

(** Evaluates to the actual list of dependencies, ignoring aliases, and
    registers them as the action dependencies. *)
val unnamed : expander:Expander.t -> Dep_conf.t list -> unit Build.t

(** Evaluates to the actual list of dependencies, ignoring aliases, and
    registers them as the action dependencies.

    It returns bindings that are later used for action expansion. *)
val named :
  expander:Expander.t -> Dep_conf.t Bindings.t -> Path.t Bindings.t Build.t
