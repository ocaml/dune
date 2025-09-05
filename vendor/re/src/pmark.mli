type t = private int

val equal : t -> t -> bool
val compare : t -> t -> int
val gen : unit -> t
val pp : t Fmt.t

module Set : sig
  include Set.S with type elt = t

  val to_list : t -> elt list
end
