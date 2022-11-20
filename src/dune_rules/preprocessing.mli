(** Preprocessing of OCaml source files *)

open Import

val pped_modules_map :
     Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
  -> Ocaml.Version.t
  -> (Module.t -> Module.t) Staged.t

val make :
     Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> lint:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocess:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocessor_deps:Dep_conf.t list
  -> instrumentation_deps:Dep_conf.t list
  -> lib_name:Lib_name.Local.t option
  -> scope:Scope.t
  -> Pp_spec.t

(** Get a path to a cached ppx driver with some extra flags for cookies. *)
val get_ppx_driver :
     Super_context.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * string list) Action_builder.t

val gen_rules : Super_context.t -> string list -> unit Memo.t

val chdir : Action_unexpanded.t -> Action_unexpanded.t

val action_for_pp_with_target :
     sandbox:Sandbox_config.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> action:Action_unexpanded.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t

val ppx_exe :
  Context.t -> scope:Scope.t -> Lib_name.t -> Path.Build.t Resolve.Memo.t
