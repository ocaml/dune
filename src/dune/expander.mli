(** An expander is able to expand any dune template. It has two modes of
  expansion:

    1. Static. In this mode it will only expand variables that do not introduce
    dependncies

    2. Dynamic. In this mode, the expander will record dependencies that are
    introduced by forms it has failed to expand. Later, these dependenceis can
    be filled for a full expansion.*)
open Stdune

type t

val bindings : t -> Pform.Map.t

val scope : t -> Scope.t

val dir : t -> Path.Build.t

val make :
     scope:Scope.t
  -> context:Context.t
  -> lib_artifacts:Artifacts.Public_libs.t
  -> bin_artifacts_host:Artifacts.Bin.t
  -> t

val set_env : t -> var:string -> value:string -> t

val hide_env : t -> var:string -> t

val set_dir : t -> dir:Path.Build.t -> t

val set_scope : t -> scope:Scope.t -> t

val set_bin_artifacts : t -> bin_artifacts_host:Artifacts.Bin.t -> t

val set_lookup_module :
     t
  -> lookup_module:
    (   dir:Path.Build.t
     -> Module_name.t
     -> (Path.Build.t Obj_dir.t * Module.t) option)
  -> t

val set_lookup_library :
     t
  -> lookup_library:
    (dir:Path.Build.t -> Lib_name.t -> Dune_file.Library.t option)
  -> t

val add_bindings : t -> bindings:Pform.Map.t -> t

val extend_env : t -> env:Env.t -> t

val expand :
  t -> mode:'a String_with_vars.Mode.t -> template:String_with_vars.t -> 'a

val expand_path : t -> String_with_vars.t -> Path.t

val expand_str : t -> String_with_vars.t -> string

val resolve_binary :
  t -> loc:Loc.t option -> prog:string -> (Path.t, Import.fail) Result.t

type reduced_var_result =
  | Unknown
  | Restricted
  | Expanded of Value.t list

val expand_with_reduced_var_set :
  context:Context.t -> reduced_var_result String_with_vars.expander

module Resolved_forms : sig
  type t

  (* Failed resolutions *)
  val failures : t -> Import.fail list

  (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
  val lib_deps : t -> Lib_deps_info.t

  (* Static deps from %{...} variables. For instance %{exe:...} *)
  val sdeps : t -> Path.Set.t

  (* Dynamic deps from %{...} variables. For instance %{read:...} *)
  val ddeps : t -> (unit, Value.t list) Build.t String.Map.t

  val empty : unit -> t
end

module Targets : sig
  type static =
    { targets : Path.Build.t list
    ; multiplicity : Dune_file.Rule.Targets.Multiplicity.t
    }

  type t =
    | Static of static
    | Infer
    | Forbidden of string  (** context *)
end

val with_record_deps :
     t
  -> Resolved_forms.t
  -> targets_written_by_user:Targets.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> map_exe:(Path.t -> Path.t)
  -> c_flags:(dir:Path.Build.t -> (unit, string list) Build.t C.Kind.Dict.t)
  -> t

val with_record_no_ddeps :
     t
  -> Resolved_forms.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> map_exe:(Path.t -> Path.t)
  -> c_flags:(dir:Path.Build.t -> (unit, string list) Build.t C.Kind.Dict.t)
  -> t

val add_ddeps_and_bindings :
     t
  -> dynamic_expansions:Value.t list String.Map.t
  -> deps_written_by_user:Path.t Bindings.t
  -> t

val expand_var_exn : t -> Value.t list option String_with_vars.expander

val expand_and_eval_set :
     t
  -> Ordered_set_lang.Unexpanded.t
  -> standard:(unit, string list) Build.t
  -> (unit, string list) Build.t

val eval_blang : t -> Blang.t -> bool
