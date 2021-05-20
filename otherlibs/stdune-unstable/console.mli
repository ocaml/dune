(** Manages the console *)

(** The console is a system than can report messages and a status to the user.
    It is usually the terminal the application is connected to, however it could
    be something else. This module allow to set a global backend for the
    application as well as composing backends. *)

module Backend : sig
  module type S = sig
    (** Format and print a user message to the console *)
    val print_user_message : User_message.t -> unit

    (** Change the status line *)
    val set_status_line : User_message.Style.t Pp.t option -> unit

    (** Print a message if the backend does not display the status line. This is
        needed so that the important status changes show up even when a [dumb]
        terminal backend is used. *)
    val print_if_no_status_line : User_message.Style.t Pp.t -> unit

    (** Reset the log output *)
    val reset : unit -> unit
  end

  type t = (module S)

  val set : t -> unit

  (** [compose a b] produce a backend that sends message to both [a] and [b]
      backends. *)
  val compose : t -> t -> t

  (** A dumb backend that hides the status line and simply dump the messages to
      the terminal *)
  val dumb : t

  (** A backend that just displays the status line in the terminal *)
  val progress : t
end

(** The main backend for the application *)
include Backend.S

(** [print paragraphs] is a short-hand for:

    {[ print_user_message (User_message.make paragraphs) ]} *)
val print : User_message.Style.t Pp.t list -> unit

module Status_line : sig
  (** This module allows to buffer status updates so that they don't slow down
      the application *)

  (** The current status line *)
  type t = User_message.Style.t Pp.t option

  (** Change the status line generator to a "live" value that's updated
      continuously, such as a progress indicator. This message is not shown when
      a "dumb" terminal backend is in use. *)
  val set_live : (unit -> t) -> unit

  (** Set the status line to a fixed value. Unlike with [set_live], this text is
      printed even if a dumb console backend is in use. *)
  val set_constant : t -> unit

  (** [set_live_temporarily status f] sets the status line to a given live value
      for the duration of [f] and then reverts to the old value. *)
  val set_live_temporarily : (unit -> t) -> (unit -> 'a) -> 'a

  val refresh : unit -> unit
end
