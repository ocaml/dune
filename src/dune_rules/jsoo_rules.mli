(** Generate rules for js_of_ocaml *)

open Import

val build_cm :
     Compilation_context.t
  -> in_context:Js_of_ocaml.In_context.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t Memo.t

val build_exe :
     Compilation_context.t
  -> in_context:Js_of_ocaml.In_context.t
  -> src:Path.Build.t
  -> cm:Path.t list Action_builder.t
  -> promote:Rule.Promote.t option
  -> link_time_code_gen:[ `Mod of Path.t | `Lib of Lib.t ] list Memo.t
  -> unit Memo.t

val setup_separate_compilation_rules :
  Super_context.t -> string list -> unit Memo.t

val runner : string
