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

val subst
  :  OpamVariable.variable_contents Var.Map.t
  -> Package_name.t
  -> src:Path.t
  -> dst:Path.Build.t
  -> unit
