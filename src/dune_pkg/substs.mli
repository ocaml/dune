open Import

module Variable : sig
  type t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
  val encode : t Encoder.t
  val of_string : string -> t
end

module Var : sig
  type t =
    { package : Package_name.t option
    ; variable : Variable.t
    }

  val compare : t -> t -> Ordering.t

  include Comparable_intf.S with type key := t

  val to_dyn : t -> Dyn.t
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
    :  (Var.t -> OpamVariable.variable_contents option Monad.t)
    -> Package_name.t
    -> src:Path.t
    -> dst:Path.Build.t
    -> unit Monad.t
end
