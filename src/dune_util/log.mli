(** Log file *)
open Stdune

module File : sig
  type t =
    | Default
    | No_log_file
    | This of Path.t
    | Out_channel of out_channel
end

(** Initialise the log file *)
val init : ?file:File.t -> unit -> unit

(** Initialise this module with a disabled logger, i.e. swallowing error
    messages. *)
val init_disabled : unit -> unit

(** Print the message only the log file (despite verbose mode) if it's set *)
val log : (unit -> User_message.Style.t Pp.t list) -> unit

(** Print an informative message in the log *)
val info_user_message : User_message.t -> unit

(** [info paragraphs] is a short-hand for:

    {[
      info_user_message (User_message.make paragraphs)
    ]} *)
val info : User_message.Style.t Pp.t list -> unit

(** Print an executed command in the log *)
val command
  :  command_line:string
  -> output:string
  -> exit_status:Unix.process_status
  -> unit

(** Whether we are running in verbose mode *)
val verbose : bool ref
