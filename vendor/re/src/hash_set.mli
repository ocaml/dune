type t

val create : unit -> t
val is_empty : t -> bool
val add : t -> int -> unit
val mem : t -> int -> bool
val clear : t -> unit
val pp : t Fmt.t
