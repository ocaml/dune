open Stdune

(** Manages the console *)

(** The console is a system than can report messages and a status to the user.
    It is usually the terminal the application is connected to, however it could
    be something else. This module allow to set a global backend for the
    application as well as composing backends. *)

module Backend : sig
  type t

  val set : t -> unit

  (** [compose a b] produce a backend that sends message to both [a] and [b]
      backends. *)
  val compose : t -> t -> t

  (** A dumb backend that hides the status line and simply dump the messages to
      the terminal. *)
  val dumb : t

  (** A backend that just displays the status line in the terminal. *)
  val progress : unit -> t

  (** A backend that uses Notty to display the status line in the terminal. *)
  val tui : unit -> t

  val spawn_thread : ((unit -> unit) -> unit) Fdecl.t
end

(** Format and print a user message to the console *)
val print_user_message : User_message.t -> unit

(** Reset the log output and (try) to remove the history *)
val reset_flush_history : unit -> unit

(** Reset the log output *)
val reset : unit -> unit

val finish : unit -> unit

(** [print paragraphs] is a short-hand for:

    {[
      print_user_message (User_message.make paragraphs)
    ]} *)
val print : User_message.Style.t Pp.t list -> unit

(** [printf fmt] is a convenient function for debugging. It formats a string and
    then print it raw followed by a newline. It is the same as:

    {[
      print [Pp.verbatim (sprintf fmt ...)]
    ]}

    For properly formatted output you should use [print]. *)
val printf : ('a, unit, string, unit) format4 -> 'a

module Status_line : sig
  (** Status line management *)

  (** The current status line *)
  type t =
    | Live of (unit -> User_message.Style.t Pp.t)
        (** A "live" value that's updated continuously, such as a progress
            indicator. This message is not shown when a "dumb" terminal backend
            is in use. *)
    | Constant of User_message.Style.t Pp.t
        (** A fixed value. Unlike with [Live], this text is printed even if a
            dumb console backend is in use. *)

  val set : t -> unit

  (** Clear the current status line *)
  val clear : unit -> unit

  type overlay

  (** Add an overlay on top of the current status line. [set] and [clear] remove
      any active overlay. *)
  val add_overlay : t -> overlay

  (** Remove an overlay if it is still active. Do nothing otherwise. *)
  val remove_overlay : overlay -> unit

  (** [with_overlay t ~f] is the same as:

      {[
        let id = add_overlay t in
        Exn.protect f ~finally:(fun () -> remove_overlay id)
      ]} *)
  val with_overlay : t -> f:(unit -> 'a) -> 'a

  val refresh : unit -> unit
end
