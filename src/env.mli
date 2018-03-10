open Import

module Var : sig
  type t = string
  val compare : t -> t -> Ordering.t
end

type t

module Map : Map.S with type key = Var.t

val initial : unit -> t

val to_unix : t -> string array

val get_var : t -> Var.t -> string option

val extend : t -> vars:string Map.t -> t
