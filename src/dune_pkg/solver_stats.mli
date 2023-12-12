open Import

type t = { expanded_variables : Variable_name.Set.t }

module Updater : sig
  type stats = t
  type t

  val init : unit -> t
  val snapshot : t -> stats
  val expand_variable : t -> Variable_name.t -> unit
  val wrap_env : t -> OpamFilter.env -> OpamFilter.env
end

module Expanded_variable_bindings : sig
  type t =
    { variable_values : (Variable_name.t * Variable_value.t) list
    ; unset_variables : Variable_name.t list
    }

  val empty : t
  val is_empty : t -> bool
  val of_variable_set : Variable_name.Set.t -> Solver_env.t -> t
  val decode : t Decoder.t
  val encode : t -> Dune_lang.t list
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_solver_env : t -> Solver_env.t
end
