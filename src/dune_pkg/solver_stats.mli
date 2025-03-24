open Import

type t = { expanded_variables : Package_variable_name.Set.t }

module Updater : sig
  type stats = t
  type t

  val init : unit -> t
  val snapshot : t -> stats
  val expand_variable : t -> Package_variable_name.t -> unit
  val wrap_env : t -> OpamFilter.env -> OpamFilter.env
end

module Expanded_variable_bindings : sig
  type t =
    { variable_values : (Package_variable_name.t * Variable_value.t) list
    ; unset_variables : Package_variable_name.t list
    }

  val empty : t
  val is_empty : t -> bool
  val of_variable_set : Package_variable_name.Set.t -> Solver_env.t -> t
  val decode : t Decoder.t
  val encode : t -> Dune_lang.t list
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_solver_env : t -> Solver_env.t

  (** [validate_against_solver_env t solver_env] checks that each variable in
      common between [t] and [solver_env] is assigned the same value, and that
      all the unset variables in [t] are not assigned a value in [solver_env]. *)
  val validate_against_solver_env : t -> Solver_env.t -> unit
end
