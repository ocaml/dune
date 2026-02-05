type t

val create : Unix.file_descr -> t
val close : t -> unit Fiber.t
val write : t -> Csexp.t list -> (unit, [ `Closed ]) result Fiber.t
val read : t -> Csexp.t option Fiber.t
