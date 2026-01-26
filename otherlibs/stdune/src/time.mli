type t

val start : t
val now : unit -> t
val to_secs : t -> float
val to_ns : t -> int
val of_epoch_secs : float -> t
val of_ns : int -> t

module Span : sig
  type t

  val zero : t
  val max : t -> t -> t
  val compare : t -> t -> Ordering.t
  val of_secs : float -> t
  val to_secs : t -> float
  val of_ns : int -> t
  val to_ns : t -> int
  val add : t -> t -> t
  val diff : t -> t -> t
end

val add : t -> Span.t -> t
val diff : t -> t -> Span.t
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
