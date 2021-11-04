(** Generate rules for js_of_ocaml *)
open! Dune_engine

open! Stdune
open Import

val build_cm :
     Compilation_context.t
  -> in_buildable:Js_of_ocaml.In_buildable.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t Memo.Build.t

val build_exe :
     Compilation_context.t
  -> in_buildable:Js_of_ocaml.In_buildable.t
  -> src:Path.Build.t
  -> cm:Path.t list Action_builder.t
  -> promote:Rule.Promote.t option
  -> link_time_code_gen:[ `Mod of Path.t | `Lib of Lib.t ] list Memo.Build.t
  -> unit Memo.Build.t

val setup_separate_compilation_rules :
  Super_context.t -> string list -> unit Memo.Build.t

val runner : string
