module type S = sig
  type t
  type u

  val create : unit -> t
  val read : t -> u
  val add : t -> u -> unit
  val reset : t -> unit
end

include S with type u := int

val incr : t -> unit

module Timer : sig
  include S with type u := Time.Span.t

  type start

  val start : unit -> start
  val stop : t -> start -> unit
end
