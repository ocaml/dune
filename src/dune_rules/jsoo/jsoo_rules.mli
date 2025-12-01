(** Generate rules for js_of_ocaml *)

open Import

module Config : sig
  type t

  val all : t list
end

module Version : sig
  type t = int * int

  val of_string : string -> t option
  val compare : t -> t -> Ordering.t
end

val build_cm
  :  Compilation_context.t
  -> dir:Path.Build.t
  -> in_context:Js_of_ocaml.In_context.t
  -> mode:Js_of_ocaml.Mode.t
  -> src:Path.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> deps:Module.t list Action_builder.t
  -> config:Config.t option
  -> Action.Full.t Action_builder.With_targets.t

val build_exe
  :  Compilation_context.t
  -> loc:Loc.t
  -> in_context:Js_of_ocaml.In_context.t
  -> src:Path.Build.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> top_sorted_modules:Module.t list Action_builder.t
  -> promote:Rule_mode.Promote.t option
  -> linkall:bool Action_builder.t
  -> link_time_code_gen:Link_time_code_gen_type.t Resolve.t
  -> jsoo_mode:Js_of_ocaml.Mode.t
  -> unit Memo.t

val setup_separate_compilation_rules : Super_context.t -> string list -> unit Memo.t
val runner : string

val js_of_ocaml_runtest_alias
  :  dir:Path.Build.t
  -> mode:Js_of_ocaml.Mode.t
  -> Alias.Name.t Memo.t

val jsoo_env
  :  dir:Path.Build.t
  -> mode:Js_of_ocaml.Mode.t
  -> string list Action_builder.t Js_of_ocaml.Env.t Memo.t

val jsoo_enabled
  :  eval:(Blang.t -> bool Memo.t)
  -> dir:Path.Build.t
  -> in_context:Js_of_ocaml.In_context.t Js_of_ocaml.Mode.Pair.t
  -> mode:Js_of_ocaml.Mode.t
  -> bool Memo.t

val jsoo_enabled_modes
  :  expander:Expander.t
  -> dir:Path.Build.t
  -> in_context:Js_of_ocaml.In_context.t Js_of_ocaml.Mode.Pair.t
  -> Js_of_ocaml.Mode.Set.t Memo.t

val jsoo_is_whole_program
  :  Super_context.t
  -> dir:Path.Build.t
  -> in_context:Js_of_ocaml.In_context.t Js_of_ocaml.Mode.Pair.t
  -> Js_of_ocaml.Mode.Set.t Memo.t
