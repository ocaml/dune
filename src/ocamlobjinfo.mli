(** Parse ocamlobjinfo output *)
open Stdune

type t = Module.Name.Set.t Ml_kind.Dict.t

val pp : t Fmt.t

val load : ocamlobjinfo:Path.t -> unit:Path.t -> t Fiber.t

(** For testing only *)
val parse : string -> t
