open Stdune

type t

val bindings : t -> Pform.Map.t
val scope : t -> Scope.t
val dir : t -> Path.t

val make
  :  scope:Scope.t
  -> context:Context.t
  -> artifacts:Artifacts.t
  -> artifacts_host:Artifacts.t
  -> cxx_flags:string list
  -> t

val set_env : t -> var:string -> value:string -> t

val set_dir : t -> dir:Path.t -> t

val update
  :  t
  -> dir:Path.t
  -> scope:Scope.t
  -> env:Env.t
  -> add_bindings:Pform.Map.t
  -> t

type var_expander =
  (Value.t list, Pform.Expansion.t) result option String_with_vars.expander

val expand
  :  t
  -> mode:'a String_with_vars.Mode.t
  -> template:String_with_vars.t
  -> 'a

module Resolved_forms : sig
  type t

  (* Failed resolutions *)
  val failures : t -> Import.fail list

  (* All "name" for %{lib:name:...}/%{lib-available:name} forms *)
  val lib_deps : t -> Lib_deps_info.t

  (* Static deps from %{...} variables. For instance %{exe:...} *)
  val sdeps    : t -> Path.Set.t

  (* Dynamic deps from %{...} variables. For instance %{read:...} *)
  val ddeps    : t -> (unit, Value.t list) Build.t String.Map.t

  val empty : unit -> t
end

type targets =
  | Static of Path.t list
  | Infer
  | Alias

val with_record_deps
  :  t
  -> Resolved_forms.t
  -> read_package:(Package.t -> (unit, string option) Build.t)
  -> dep_kind:Lib_deps_info.Kind.t
  -> targets_written_by_user:targets
  -> map_exe:(Path.t -> Path.t)
  -> t

val expand_ddeps_and_bindings
  :  t
  -> dynamic_expansions:Value.t list String.Map.t
  -> deps_written_by_user:Path.t Bindings.t
  -> t

val expand_var_exn : t -> Value.t list option String_with_vars.expander
