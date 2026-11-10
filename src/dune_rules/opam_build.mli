(** Opaque execution of a selected package recipe. Source preparation and
    dependency selection belong to callers. *)

open Import

module Variable : sig
  type value = OpamVariable.variable_contents =
    | B of bool
    | S of string
    | L of string list

  type t = Package_variable_name.t * value

  val dune_value : value -> Value.t list
end

module Paths : sig
  type 'a t =
    { source_dir : 'a
    ; target_dir : 'a
    ; extra_sources : 'a
    ; name : Package.Name.t
    ; install_roots : 'a Install.Roots.t Lazy.t
    ; install_paths : 'a Install.Paths.t Lazy.t
    ; prefix : 'a
    }

  val of_root : Package.Name.t -> root:'a -> relative:('a -> string -> 'a) -> 'a t
  val map_path : 'a t -> f:('a -> 'b) -> 'b t
  val with_prefix : Path.t t -> Path.t -> Path.t t
  val extra_source : Path.t t -> Path.Local.t -> Path.t
  val extra_source_build : Path.Build.t t -> Path.Local.t -> Path.Build.t
  val install_cookie : Path.t t -> Path.t
  val install_paths : 'a t -> 'a Install.Paths.t
  val install_roots : 'a t -> 'a Install.Roots.t
  val target_dir : 'a t -> 'a
end

module Install_cookie : sig
  module Gen : sig
    type 'files t =
      { files : 'files
      ; variables : Variable.t list
      }
  end

  type t = Path.t list Section.Map.t Gen.t

  val load_exn : Path.t -> t
end

module Dependency : sig
  type t =
    { paths : Path.t Paths.t
    ; files : Path.t list Section.Map.t
    ; variables : Variable.value Package_variable_name.Map.t
    ; exported_env : String_with_vars.t Dune_lang.Action.Env_update.t list
    ; export_paths : Path.t Paths.t
    }

  val binaries : t -> Path.t list
end

val variables
  :  name:Package.Name.t
  -> version:Package_version.t
  -> dev:bool
  -> Variable.value Package_variable_name.Map.t

module Value_list_env : sig
  type t = Value.t list Env.Map.t

  val global : t Lazy.t
  val to_env : t -> Env.t
  val string_of_env_values : Value.t list -> string
  val extend_concat_path : t -> t -> t
  val add_path : t -> Env.Var.t -> Path.t -> t
end

module Env_update : sig
  include module type of Dune_lang.Action.Env_update

  val set : Value_list_env.t -> string t -> Value_list_env.t
end

module Action_expander : sig
  module Expander : sig
    type t =
      { name : Package.Name.t
      ; paths : Path.t Paths.t
      ; artifacts : Path.t Filename.Map.t Memo.t
      ; depends :
          (Variable.value Package_variable_name.Map.t * Path.t Paths.t) Package.Name.Map.t
            Memo.t
      ; files : Path.t list Section.Map.t Package.Name.Map.t Memo.t
      ; depexts : Dune_pkg.Lock_dir.Depexts.t list
      ; context : Context_name.t
      ; path : Path.t list
      ; version : Package_version.t
      ; env : Value_list_env.t
      }

    val filtered_depexts : t -> string list Memo.t
  end

  val expand : Dune_lang.Action.t -> expander:Expander.t -> Action.t Memo.t

  val exported_env
    :  Expander.t
    -> String_with_vars.t Env_update.t
    -> string Env_update.t Memo.t
end

module Recipe : sig
  type t =
    { build : Dune_lang.Action.t option
    ; install : Dune_lang.Action.t option
    ; depexts : Dune_pkg.Lock_dir.Depexts.t list
    ; exported_env : String_with_vars.t Env_update.t list
    }
end

(** All source copies are already prepared files. Destinations are relative
    to the supplied working directory, not persistent source targets. *)
val execute
  :  expander:Action_expander.Expander.t
  -> write_paths:Path.Build.t Paths.t
  -> source_files:(Path.t * Path.Local.t) list Action_builder.t
  -> build:Action.Full.t Action_builder.With_targets.t list
  -> install:Action.Full.t Action_builder.With_targets.t list
  -> Action.Full.t Action_builder.With_targets.t
