open Stdune

type t

val tar : t
val zip : t
val choose_for_filename : string -> t option
val extract : t -> archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t
