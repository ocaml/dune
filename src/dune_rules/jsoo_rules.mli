(** Generate rules for js_of_ocaml *)
open! Dune_engine

open! Stdune
open Import

val build_cm :
     Compilation_context.t
  -> js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.t Build.With_targets.t list

val build_exe :
     Compilation_context.t
  -> js_of_ocaml:Dune_file.Js_of_ocaml.t
  -> src:Path.Build.t
  -> cm:Path.t list Build.t
  -> flags:Command.Args.dynamic Command.Args.t
  -> promote:Rule.Promote.t option
  -> unit

val setup_separate_compilation_rules : Super_context.t -> string list -> unit

val standard : Super_context.t -> string list
