(** Interpret dependencies written in Dune files *)
open! Stdune

open! Dune_engine

(** Evaluates unnamed dependency specifications. *)
val unnamed :
     expander:Expander.t
  -> Dep_conf.t list
  -> unit Action_builder.t * Sandbox_config.t

(** Evaluates named dependency specifications. Return the action build that
    register dependencies as well as an expander that can be used to expand to
    expand variables from the bindings. *)
val named :
     expander:Expander.t
  -> Dep_conf.t Bindings.t
  -> unit Action_builder.t * Expander.t * Sandbox_config.t
