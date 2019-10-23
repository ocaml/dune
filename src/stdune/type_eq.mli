(** Equality between types *)

type ('a, 'b) t = T : ('a, 'a) t

val cast : ('a, 'b) t -> 'a -> 'b

module Id : sig
  type ('a, 'b) eq
  type 'a t

  val create : unit -> 'a t

  val same : 'a t -> 'b t -> ('a, 'b) eq option
end with type ('a, 'b) eq := ('a, 'b) t
