(** Like [Lazy], but is allowed to run the computation inside a fiber *)

type 'a t

val create : (unit -> 'a Fiber.t) -> 'a t
val force : 'a t -> 'a Fiber.t
