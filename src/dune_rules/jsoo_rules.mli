(** Generate rules for js_of_ocaml *)
open! Dune_engine

open! Stdune
open Import

val build_cm :
     Compilation_context.t
  -> js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.t Action_builder.With_targets.t Memo.Build.t option

val build_exe :
     Compilation_context.t
  -> js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> src:Path.Build.t
  -> cm:Path.t list Action_builder.t
  -> flags:Command.Args.any Command.Args.t
  -> promote:Rule.Promote.t option
  -> unit Memo.Build.t

val setup_separate_compilation_rules :
  Super_context.t -> string list -> unit Memo.Build.t

val standard : Super_context.t -> string list
