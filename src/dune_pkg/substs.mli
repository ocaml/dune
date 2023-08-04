open Stdune

module Variable : sig
  type t

  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
  val encode : t -> Dune_lang.t
  val of_string : string -> t
end

module Var : sig
  type t =
    { package : Dune_lang.Package_name.t option
    ; variable : Variable.t
    }

  val compare : t -> t -> Ordering.t

  include Comparable_intf.S with type key := t

  val to_dyn : t -> Dyn.t
end

module Map = Var.Map

val subst
  :  OpamVariable.variable_contents Map.t
  -> Dune_lang.Package_name.t
  -> path:Path.t
  -> target:Path.t
  -> unit
