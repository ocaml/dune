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
