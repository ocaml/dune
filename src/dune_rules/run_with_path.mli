(**  Like a regular [run] action, but delays expanding its argument until
     action execution *)

open Import

(* CR-someday rgrinberg: move this somewhere better *)
val depexts_hint : string list -> 'a Pp.t option

module Spec : sig
  type 'path chunk =
    | String of string
    | Path of 'path

  type 'path arg = 'path chunk Array.Immutable.t
end

val action
  :  pkg:Package.Name.t * Loc.t
  -> depexts:string list
  -> Action.Prog.t
  -> Path.t Spec.arg Array.Immutable.t
  -> ocamlfind_destdir:Path.t
  -> Action.t
