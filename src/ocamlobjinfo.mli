(** Parse ocamlobjinfo output *)
open Stdune

type t = Module.Name.Set.t Ml_kind.Dict.t

val pp : t Fmt.t

val rules
  :  dir:Path.t
  -> ctx:Context.t
  -> unit:Path.t
  -> (_, Action.t) Build.t * (_, t) Build.t

(** For testing only *)
val parse : string -> t
