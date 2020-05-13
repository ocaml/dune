(** Preprocessing of OCaml source files *)

open! Stdune
open! Import

(** Preprocessing object *)
type t

val dummy : t

val make :
     Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> lint:Dune_file.Preprocess_map.t
  -> preprocess:Dune_file.Preprocess_map.t
  -> preprocessor_deps:Dep_conf.t list
  -> lib_name:Lib_name.Local.t option
  -> scope:Scope.t
  -> t

(** Setup the preprocessing rules for the following modules and returns the
    translated modules *)
val pp_module : t -> ?lint:bool -> Module.t -> Module.t

(** Preprocess a single module, using the configuration for the given module
    name. *)
val pp_module_as : t -> ?lint:bool -> Module_name.t -> Module.t -> Module.t

(** Get a path to a cached ppx driver with some extra flags for cookies. *)
val get_ppx_driver :
     Super_context.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * string list) Or_exn.t

val gen_rules : Super_context.t -> string list -> unit

val chdir : Action_unexpanded.t -> Action_unexpanded.t

val action_for_pp :
     dep_kind:Lib_deps_info.Kind.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> action:Action_unexpanded.t
  -> src:Path.Build.t
  -> target:Path.Build.t option
  -> Action.t Build.With_targets.t

val ppx_exe :
  Super_context.t -> scope:Scope.t -> Lib_name.t -> Path.Build.t Or_exn.t
