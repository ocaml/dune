(** Workspaces definitions *)

open Import

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
      ; env : Dune_env.Stanza.t
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
      ; switch : string
      ; root : string option
      }
  end

  module Default : sig
    type t = Common.t
  end

  type t =
    | Default of Default.t
    | Opam of Opam.t

  val loc : t -> Loc.t

  val name : t -> Context_name.t

  val env : t -> Dune_env.Stanza.t

  val host_context : t -> Context_name.t option

  val to_dyn : t -> Dyn.t
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
  ; env : Dune_env.Stanza.t
  ; config : Dune_config.t
  }

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

val hash : t -> int

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
val update_execution_parameters :
  t -> Execution_parameters.t -> Execution_parameters.t

(** All the build contexts defined in the workspace. *)
val build_contexts : t -> Build_context.t list
