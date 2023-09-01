(** Parse ocamlobjinfo output *)

open Import

type t = Module_name.Unique.Set.t Ml_kind.Dict.t

val to_dyn : t -> Dyn.t

val rules
  :  Ocaml_toolchain.t
  -> dir:Path.Build.t
  -> sandbox:Sandbox_config.t option
  -> unit:Path.t
  -> Action.Full.t Action_builder.With_targets.t * t Action_builder.t

(** For testing only *)
val parse : string -> t
