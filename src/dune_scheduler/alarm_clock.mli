open Stdune

type t

val create : Event.Queue.t -> Time.Span.t -> t

type alarm

val await : alarm -> [ `Finished | `Cancelled ] Fiber.t
val cancel : t -> alarm -> unit
val sleep : t -> Time.Span.t -> alarm
val close : t -> unit
