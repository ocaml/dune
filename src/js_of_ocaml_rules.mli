(** Generate rules for js_of_ocaml *)

open Jbuild_types

val build_cm
  :  Super_context.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> unit

val build_exe
  :  Super_context.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> requires:(unit, Lib.t list) Build.t
  -> top_closed_cm_files:(unit, Path.t list) Build.t
  -> unit

val setup_separate_compilation_rules : Super_context.t -> unit


