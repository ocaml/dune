open Stdune

type t = Module.Name.Set.t Ml_kind.Dict.t

val load : ocamlobjinfo:Path.t -> unit:Path.t -> t Fiber.t
