(** Log file *)

(** Initialise the log file *)
val init : ?path:Path.t -> unit -> unit

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
