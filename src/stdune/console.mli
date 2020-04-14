(** Manages the console *)

(** The console is a system than can report messages and a status to the user.
    It is usually the terminal the application is connected to, however it could
    be something else. This module allow to set a global backend for the
    application as well as composing backends. *)

module Backend : sig
  module type S = sig
    val print_user_message : User_message.t -> unit
    (** Format and print a user message to the console *)

    val set_status_line : User_message.Style.t Pp.t option -> unit
    (** Change the status line *)

    val reset : unit -> unit
    (** Reset the log output *)
  end

  type t = (module S)

  val set : t -> unit

  val compose : t -> t -> t
  (** [compose a b] produce a backend that sends message to both [a] and [b]
      backends. *)

  val dumb : t
  (** A dumb backend that hides the status line and simply dump the messages to
      the terminal *)

  val progress : t
  (** A backend that just displays the status line in the terminal *)
end

(** The main backend for the application *)
include Backend.S

val print : User_message.Style.t Pp.t list -> unit
(** [print paragraphs] is a short-hand for:

    {[ print_user_message (User_message.make paragraphs) ]} *)

module Status_line : sig
  (** This module allows to buffer status updates so that they don't slow down
      the application *)

  (** Function that produces the current status line *)
  type t = unit -> User_message.Style.t Pp.t option

  val set : t -> unit
  (** Change the status line generator *)

  val set_temporarily : t -> (unit -> 'a) -> 'a

  val refresh : unit -> unit
end
