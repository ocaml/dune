type t

val now : unit -> t
val to_secs : t -> float
val of_epoch_secs : float -> t

module Span : sig
  type t

  val zero : t
  val max : t -> t -> t
  val compare : t -> t -> Ordering.t
  val of_secs : float -> t
  val add : t -> t -> t
  val diff : t -> t -> t
  val to_secs : t -> float
end

val add : t -> Span.t -> t
val diff : t -> t -> Span.t
