open Import
module Non_evaluated_rule = Rule

module Rule : sig
  type t = private
    { id : Rule.Id.t
    ; deps : Dep.Set.t
    ; (* [expanded_deps] skips over non-file dependencies, such as: environment
         variables, universe, glob listings, sandbox requirements *)
      expanded_deps : Path.Set.t
    ; targets : Targets.Validated.t
    ; action : Action.t
    }
end

(** Used by Jane Street internal rules. *)
val evaluate_rule : Non_evaluated_rule.t -> Rule.t Memo.t

val eval : recursive:bool -> request:unit Action_builder.t -> Rule.t list Memo.t
