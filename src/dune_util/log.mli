(** Log file *)
open Stdune

module File : sig
  type t =
    | Default
    | No_log_file
    | This of Path.t
end

val init : ?file:File.t -> unit -> unit
(** Initialise the log file *)

val init_disabled : unit -> unit
(** Initialise this module with a disabled logger, i.e. swallowing error
    messages. *)

val info_user_message : User_message.t -> unit
(** Print an informative message in the log *)

val info : User_message.Style.t Pp.t list -> unit
(** [info paragraphs] is a short-hand for:

    {[ info_user_message (User_message.make paragraphs) ]} *)

val command :
     command_line:string
  -> output:string
  -> exit_status:Unix.process_status
  -> unit
(** Print an executed command in the log *)

val verbose : bool ref
(** Whether we are running in verbose mode *)
