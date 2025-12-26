(** Log file *)

module Message : sig
  type level =
    [ `Warn
    | `Info
    | `Verbose
    ]

  type t =
    { level : level
    ; message : string
    ; args : (string * Dyn.t) list
    }

  val create : level -> string -> (string * Dyn.t) list -> t
end

module File : sig
  type t =
    | Redirect of (Message.t -> unit)
    | No_log_file
    | Stderr
    | Both of t * t
end

(** Initialise the log file *)
val init : File.t -> unit

(** Print the message only the log file (despite verbose mode) if it's set *)
val log : (unit -> Message.t) -> unit

val set_forward_verbose : (string -> (string * Dyn.t) list -> unit) -> unit
val info : string -> (string * Dyn.t) list -> unit
val warn : string -> (string * Dyn.t) list -> unit

(** Print an executed command in the log *)
val command
  :  command_line:string
  -> output:string
  -> exit_status:Unix.process_status
  -> unit

val verbose_message : string -> (string * Dyn.t) list -> unit

(** Whether we are running in verbose mode *)
val verbose : bool ref
