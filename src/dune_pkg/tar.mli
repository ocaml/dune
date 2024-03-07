open Stdune

val bin : Path.t Lazy.t
val extract : archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t
