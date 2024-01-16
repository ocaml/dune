(** Workspaces definitions *)

open Import

module Lock_dir : sig
  type t =
    { path : Path.Source.t
    ; version_preference : Dune_pkg.Version_preference.t option
    ; solver_env : Dune_pkg.Solver_env.t option
    ; unset_solver_vars : Dune_pkg.Variable_name.Set.t option
    ; repositories : (Loc.t * Dune_pkg.Pkg_workspace.Repository.Name.t) list
    ; constraints : Dune_lang.Package_dependency.t list
    }

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module Lock_dir_selection : sig
  type t

  val eval
    :  t
    -> dir:Path.Source.t
    -> f:Value.t list Memo.t String_with_vars.expander
    -> Path.Source.t Memo.t
end

module Context : sig
  module Target : sig
    type t =
      | Native
      | Named of Context_name.t

    val equal : t -> t -> bool
  end

  module Common : sig
    type t =
      { loc : Loc.t
      ; profile : Profile.t
      ; targets : Target.t list
      ; env : Dune_env.t option
      ; toolchain : Context_name.t option
      ; name : Context_name.t
      ; host_context : Context_name.t option
      ; paths : (string * Ordered_set_lang.t) list
      ; fdo_target_exe : Path.t option
      (** By default Dune builds and installs dynamically linked foreign
          archives (usually named [dll*.so]). It is possible to disable
          this by setting [disable_dynamically_linked_foreign_archives] to
          [true] in the workspace file, in which case bytecode executables
          will be built with all foreign archives statically linked into
          the runtime system. *)
      ; dynamically_linked_foreign_archives : bool
      ; instrument_with : Lib_name.t list
      ; merlin : bool
      }
  end

  module Opam : sig
    type t =
      { base : Common.t
      (** Either a switch name or a path to a local switch. This argument
          is left opaque as we leave to opam to interpret it. *)
      ; switch : Opam_switch.t
      }
  end

  module Default : sig
    type t =
      { base : Common.t
      ; lock_dir : Lock_dir_selection.t option
      }
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  val loc : t -> Loc.t
  val name : t -> Context_name.t
  val env : t -> Dune_env.t option
  val host_context : t -> Context_name.t option
  val to_dyn : t -> Dyn.t
  val base : t -> Common.t
end

(** Representation of a workspace. The list of context is topologically sorted,
    i.e. a context always comes before the contexts where it is used as host
    context.

    The various field aggregate all of, by order of precedence:

    - the command line arguments
    - the contents of the workspace file
    - the contents of the user configuration file
    - the default values *)
type t = private
  { merlin_context : Context_name.t option
  ; contexts : Context.t list
  ; env : Dune_env.t option
  ; config : Dune_config.t
  ; repos : Dune_pkg.Pkg_workspace.Repository.t list
  ; lock_dirs : Lock_dir.t list
  ; dir : Path.Source.t
  }

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val hash : t -> int
val find_lock_dir : t -> Path.Source.t -> Lock_dir.t option
val default_repositories : Dune_pkg.Pkg_workspace.Repository.t list

module Clflags : sig
  type t =
    { x : Context_name.t option
    ; profile : Profile.t option
    ; instrument_with : Lib_name.t list option
    ; workspace_file : Path.Outside_build_dir.t option
    ; config_from_command_line : Dune_config.Partial.t
    ; config_from_config_file : Dune_config.Partial.t
    }

  (** This must be called exactly once *)
  val set : t -> unit
end

(** Default name of workspace files *)
val filename : Filename.t

val workspace : unit -> t Memo.t

(** Same as [workspace ()] except that if there are errors related to fields
    other than the ones of [config], they are not reported. *)
val workspace_config : unit -> Dune_config.t Memo.t

(** Update the execution parameters according to what is written in the
    [dune-workspace] file. *)
val update_execution_parameters : t -> Execution_parameters.t -> Execution_parameters.t

(** All the build contexts defined in the workspace. *)
val build_contexts : t -> Build_context.t list
