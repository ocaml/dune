open Import

module Variable : sig
  type t =
    | Global of Package_variable_name.t
    | Package of Package_variable.t
end

module Make (Monad : sig
    type 'a t

    module O : sig
      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    end

    module List : sig
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end) : sig
  val subst
    :  (Variable.t -> OpamVariable.variable_contents option Monad.t)
    -> Package_name.t
    -> src:Path.t
    -> dst:Path.Build.t
    -> unit Monad.t
end
