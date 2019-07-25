(** Manage printing user message and keeping progress information in the status line *)


module Display : sig

    type t =
      | Progress
      | Short
      | Verbose
      | Quiet

    val all : (string * t) list

end

val print : string -> unit

val print_user_message
  :  ?config:User_message.Print_config.t
  -> User_message.t
  -> unit

val init : Display.t -> unit

(** / *)
(** Everything below this line requires [init] to have been called earlier. *)

val update_status_line : User_message.Style.t Pp.t -> unit
(** Update the status line if the display is in progress mode. *)

val clear_status_line : unit -> unit
(** Clear the status line *)

val reset_terminal : unit -> unit
val display : unit -> Display.t
