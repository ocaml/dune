module Dune_config : sig
  (** Dune configuration (visible to the user) *)

  open Stdune
  module Display : module type of Display

  module Project_defaults : sig
    type t =
      { authors : string list option
      ; maintainers : string list option
      ; maintenance_intent : string list option
      ; license : string list option
      }

    val decode : t Dune_lang.Decoder.t
  end

  module Concurrency : sig
    type t =
      | Fixed of int
      | Auto

    val equal : t -> t -> bool
    val of_string : string -> (t, string) result
    val to_string : t -> string
  end

  module Sandboxing_preference : sig
    type t = Dune_engine.Sandbox_mode.t list
  end

  module Cache : sig
    module Toggle : sig
      type t =
        | Disabled
        | Enabled_except_user_rules
        | Enabled

      val all : (string * t) list

      val decode
        :  check:(Dune_lang.Syntax.Version.t -> unit Dune_lang.Decoder.t)
        -> t Dune_lang.Decoder.t

      val to_string : t -> string
    end

    module Storage_mode : sig
      type t = Dune_cache_storage.Mode.t option

      val all : (string * t) list
      val decode : t Dune_lang.Decoder.t
      val to_string : t -> string
    end
  end

  module Pkg_enabled : sig
    (** Configuration for Dune's package management features.
        
        - [Set (loc, `Enabled)]: Package management is explicitly enabled. Forces package 
          management to be active even if no lock directories are present.

        - [Set (loc, `Disabled)]: Package management is explicitly disabled. Forces package 
          management to be inactive even if lock directories are present.

        - [Unset]: Package management enablement is not explicitly configured.  *)
    type t =
      | Set of Loc.t * Dune_config.Config.Toggle.t
      | Unset
  end

  module Terminal_persistence : sig
    type t =
      | Preserve
      | Clear_on_rebuild
      | Clear_on_rebuild_and_flush_history

    val all : (string * t) list
  end

  module Action_output_on_success : sig
    include module type of struct
      include Dune_engine.Execution_parameters.Action_output_on_success
    end
  end

  module type S = sig
    type 'a field

    type t =
      { display : Display.t field
      ; concurrency : Concurrency.t field
      ; terminal_persistence : Terminal_persistence.t field
      ; sandboxing_preference : Sandboxing_preference.t field
      ; cache_enabled : Cache.Toggle.t field
      ; cache_reproducibility_check : Dune_cache.Config.Reproducibility_check.t field
      ; cache_storage_mode : Cache.Storage_mode.t field
      ; action_stdout_on_success : Action_output_on_success.t field
      ; action_stderr_on_success : Action_output_on_success.t field
      ; project_defaults : Project_defaults.t field
      ; pkg_enabled : Pkg_enabled.t field
      ; experimental : (string * (Loc.t * string)) list field
      }
  end

  include S with type 'a field = 'a

  module Partial : sig
    include S with type 'a field := 'a option

    val empty : t
    val superpose : t -> t -> t
    val to_dyn : t -> Dyn.t
    val equal : t -> t -> bool
  end

  (** A standard list of watch exclusions *)
  val standard_watch_exclusions : string list

  val decode : Partial.t Dune_lang.Decoder.t

  (** Decode the same fields as the one accepted in the configuration file, but
      coming from the [dune-workspace] file. The main difference is that we
      started accepting such parameters in the [dune-workspace] file starting
      from Dune 3.0.0, so the version checks are different. *)
  val decode_fields_of_workspace_file : Partial.t Dune_lang.Decoder.fields_parser

  val superpose : t -> Partial.t -> t
  val default : t
  val user_config_file : Path.t Lazy.t

  (** We return a [Partial.t] here so that the result can easily be merged with
      other sources of configurations. *)
  val load_user_config_file : unit -> Partial.t

  val load_config_file : Path.t -> Partial.t

  (** Set display mode to [Quiet] if it is [Progress], the output is not a tty
      and we are not running inside emacs. *)
  val adapt_display : t -> output_is_a_tty:bool -> t

  (** Initialises the configuration for the process *)
  val init : t -> watch:bool -> unit

  val to_dyn : t -> Dyn.t
  val hash : t -> int
  val equal : t -> t -> bool

  (** [for_scheduler config ?watch_exclusions stats_opt ~signal_watcher]
      creates a configuration for a scheduler from the user-visible Dune
      [config]. *)
  val for_scheduler
    :  t
    -> watch_exclusions:string list
    -> Dune_trace.Out.t option
    -> print_ctrl_c_warning:bool
    -> Dune_engine.Scheduler.Config.t
end
