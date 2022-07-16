(** Dune configuration (visible to the user) *)

open Stdune

module Concurrency : sig
  type t =
    | Fixed of int
    | Auto

  val of_string : string -> (t, string) result

  val to_string : t -> string
end

module Sandboxing_preference : sig
  type t = Dune_engine.Sandbox_mode.t list
end

module Cache : sig
  module Enabled : sig
    type t =
      | Enabled
      | Disabled

    val all : (string * t) list

    val decode : t Dune_lang.Decoder.t

    val to_string : t -> string
  end

  module Storage_mode : sig
    type t = Dune_cache_storage.Mode.t option

    val all : (string * t) list

    val decode : t Dune_lang.Decoder.t

    val to_string : t -> string
  end
end

module Terminal_persistence : sig
  type t =
    | Preserve
    | Clear_on_rebuild

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
    { display : Dune_engine.Scheduler.Config.Display.t field
    ; concurrency : Concurrency.t field
    ; terminal_persistence : Terminal_persistence.t field
    ; sandboxing_preference : Sandboxing_preference.t field
    ; cache_enabled : Cache.Enabled.t field
    ; cache_reproducibility_check :
        Dune_cache.Config.Reproducibility_check.t field
    ; cache_storage_mode : Cache.Storage_mode.t field
    ; action_stdout_on_success : Action_output_on_success.t field
    ; action_stderr_on_success : Action_output_on_success.t field
    }
end

include S with type 'a field = 'a

module Partial : sig
  include S with type 'a field := 'a option

  val empty : t

  val superpose : t -> t -> t

  val to_dyn : t -> Dyn.t
end

val decode : Partial.t Dune_lang.Decoder.t

(** Decode the same fields as the one accepted in the configuration file, but
    coming from the [dune-workspace] file. The main difference is that we
    started accepting such parameters in the [dune-workspace] file starting from
    Dune 3.0.0, so the version checks are different. *)
val decode_fields_of_workspace_file : Partial.t Dune_lang.Decoder.fields_parser

val superpose : t -> Partial.t -> t

val default : t

val user_config_file : Path.t

(** We return a [Partial.t] here so that the result can easily be merged with
    other sources of configurations. *)
val load_user_config_file : unit -> Partial.t

val load_config_file : Path.t -> Partial.t

(** Set display mode to [Quiet] if it is [Progress], the output is not a tty and
    we are not running inside emacs. *)
val adapt_display : t -> output_is_a_tty:bool -> t

(** Initialises the configuration for the process *)
val init : t -> unit

val to_dyn : t -> Dyn.t

val hash : t -> int

val equal : t -> t -> bool

val for_scheduler :
     t
  -> Dune_stats.t option
  -> insignificant_changes:[ `React | `Ignore ]
  -> Dune_engine.Scheduler.Config.t
