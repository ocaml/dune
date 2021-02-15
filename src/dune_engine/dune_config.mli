open Import

(** Dune configuration (visible to the user) *)

module Display : sig
  type t =
    | Progress  (** Single interactive status line *)
    | Short  (** One line per command *)
    | Verbose  (** Display all commands fully *)
    | Quiet  (** Only display errors *)

  val decode : t Dune_lang.Decoder.t

  val all : (string * t) list

  (** The console backend corresponding to the selected display mode *)
  val console_backend : t -> Console.Backend.t
end

module Concurrency : sig
  type t =
    | Fixed of int
    | Auto

  val of_string : string -> (t, string) result

  val to_string : t -> string
end

module Sandboxing_preference : sig
  type t = Sandbox_mode.t list
end

module Caching : sig
  module Mode : sig
    type t =
      | Disabled
      | Enabled

    val all : (string * t) list

    val decode : t Dune_lang.Decoder.t

    val to_string : t -> string
  end

  module Transport : sig
    type t =
      | Daemon
      | Direct

    val all : (string * t) list

    val decode : t Dune_lang.Decoder.t
  end

  module Duplication : sig
    type t = Cache.Duplication_mode.t option

    val all : (string * t) list

    val decode : t Dune_lang.Decoder.t
  end
end

module type S = sig
  type 'a field

  type t =
    { display : Display.t field
    ; concurrency : Concurrency.t field
    ; terminal_persistence : Scheduler.Config.Terminal_persistence.t field
    ; sandboxing_preference : Sandboxing_preference.t field
    ; cache_mode : Caching.Mode.t field
    ; cache_transport : Caching.Transport.t field
    ; cache_check_probability : float field
    ; cache_duplication : Caching.Duplication.t field
    ; cache_trim_period : int field
    ; cache_trim_size : int64 field
    }
end

include S with type 'a field = 'a

module Partial : S with type 'a field := 'a option

val decode : t Dune_lang.Decoder.t

val merge : t -> Partial.t -> t

val default : t

val user_config_file : Path.t

val load_user_config_file : unit -> t

val load_config_file : Path.t -> t

(** Set display mode to [Quiet] if it is [Progress], the output is not a tty and
    we are not running inside emacs. *)
val adapt_display : t -> output_is_a_tty:bool -> t

(** The global configuration for the process *)
val t : unit -> t

(** Initialises the configuration for the process *)
val init : t -> unit

val to_dyn : t -> Dyn.t

val for_scheduler : t -> Scheduler.Config.t
