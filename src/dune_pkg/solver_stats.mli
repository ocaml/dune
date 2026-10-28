open Import

type t =
  { workspace : Package_variable_name.Set.t
  ; dependencies : Package_variable_name.Set.t
  }

module Updater : sig
  type stats = t
  type t

  val init : unit -> t
  val snapshot : t -> stats
  val expand_variable : t -> scope:Solver_env.Scope.t -> Package_variable_name.t -> unit
  val wrap_env : t -> scope:Solver_env.Scope.t -> OpamFilter.env -> OpamFilter.env
end

module Expanded_variable_bindings : sig
  type stats := t
  type t

  val empty : t
  val empty_scoped : t
  val version : t -> Syntax.Version.t
  val is_empty : t -> bool
  val of_stats : stats -> Solver_env.Scoped.t -> t
  val decode : t Decoder.t
  val encode : t -> Dune_lang.t list
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_solver_env : t -> scope:Solver_env.Scope.t -> Solver_env.t

  (** Flat metadata retains its legacy validation behavior. Scoped metadata
      requires matching configuration and validates each scope independently. *)
  val validate_against_solver_env
    :  t
    -> legacy:Solver_env.t
    -> solver_env:Solver_env.Scoped.t
    -> unit

  val remove_platform_specific : t -> t
end
