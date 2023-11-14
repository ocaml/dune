open Import

type t = { expanded_variables : Solver_env.Variable.Set.t }

module Updater : sig
  type stats = t
  type t

  val init : unit -> t
  val snapshot : t -> stats
  val expand_variable : t -> Solver_env.Variable.t -> unit
end

module Expanded_variable_bindings : sig
  type t =
    { variable_values : (Solver_env.Variable.t * string) list
    ; unset_variables : Solver_env.Variable.t list
    }

  val empty : t
  val is_empty : t -> bool
  val of_variable_set : Solver_env.Variable.Set.t -> Solver_env.t -> t
  val decode : t Decoder.t
  val encode : t -> Dune_lang.t list
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val to_solver_env : t -> Solver_env.t
end
