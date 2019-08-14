(** Log file *)

module File : sig
  type t =
    | Default
    | No_log_file
    | This of Path.t
end

(** Initialise the log file *)
val init : ?file:File.t -> unit -> unit

(** Initialise this module with a disabled logger, i.e. swallowing error
  messages. *)
val init_disabled : unit -> unit

(** Print an informative message in the log *)
val info : string -> unit

val infof : ('a, Format.formatter, unit, unit) format4 -> 'a

(** Print an executed command in the log *)
val command :
     command_line:string
  -> output:string
  -> exit_status:Unix.process_status
  -> unit
