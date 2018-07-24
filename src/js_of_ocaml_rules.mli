(** Generate rules for js_of_ocaml *)

open Import

val build_cm
  :  Super_context.t
  -> dir:Path.t
  -> scope:Scope.t
  -> build:Jbuild.Buildable.t
  -> src:Path.t
  -> target:Path.t
  -> (unit, Action.t) Build.t list

val build_exe
  :  Super_context.t
  -> dir:Path.t
  -> jsoo_build:Jsoo_stanza.In_buildable.t
  -> src:Path.t
  -> requires:Lib.t list Or_exn.t
  -> (Path.t list * string list, Action.t) Build.t list

val setup_separate_compilation_rules
  :  Super_context.t
  -> string list
  -> unit
