(** Generate rules for js_of_ocaml *)

open! Stdune
open Import
open Dune_file

val build_cm :
     Compilation_context.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.t Build.t list

val build_exe :
     Compilation_context.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.Build.t
  -> cm:Path.t list Build.t
  -> flags:Command.Args.dynamic Command.Args.t
  -> promote:Dune_file.Promote.t option
  -> unit

val setup_separate_compilation_rules : Super_context.t -> string list -> unit

val standard : Super_context.t -> string list
