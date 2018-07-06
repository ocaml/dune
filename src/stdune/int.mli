type t = int
val compare : t -> t -> Ordering.t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

val of_string_exn : string -> t
