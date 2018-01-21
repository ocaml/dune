(** Generate rules for js_of_ocaml *)

open Jbuild

val build_cm
  :  Super_context.t
  -> scope:Scope.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> target:Path.t
  -> (unit, Action.t) Build.t list

val build_exe
  :  Super_context.t
  -> dir:Path.t
  -> js_of_ocaml:Js_of_ocaml.t
  -> src:Path.t
  -> ((Lib.t list * Path.t list) * string list, Action.t) Build.t list

val setup_separate_compilation_rules
  :  Super_context.t
  -> string list
  -> unit

val standard : unit -> string list
