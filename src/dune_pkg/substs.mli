open Import

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
    :  (Package_variable.t -> OpamVariable.variable_contents option Monad.t)
    -> Package_name.t
    -> src:Path.t
    -> dst:Path.Build.t
    -> unit Monad.t
end
