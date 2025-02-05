open Stdune

val bin : Path.t Lazy.t
val extract : archive:Path.t -> target:Path.t -> (unit, unit) result Fiber.t

(** [load_or_untar ~target ~archive] checks if target exists. If it does it
    returns the path. If it doesn't, it will extract [archive] as [target]. *)
val load_or_untar : target:Path.t -> archive:Path.t -> Path.t Fiber.t
