open Stdune

(** Backends may have access to an internal state. This type is used by the
    [Threaded] backends. *)
type state =
  { messages : User_message.t Queue.t
  ; mutable finish_requested : bool
  ; mutable finished : bool
  ; mutable status_line : User_message.Style.t Pp.t option
  ; mutable dirty : bool
  }

(** [Threaded] is the interface for user interfaces that are rendered in a
    separate thread. It can be used to directly construct a threaded backend.

    This module interface will be used by both the current console UI and the
    upcoming terminal UI (TUI).

    Notice that the TUI can also react to keyboard input, so it has the
    additional [handle_user_events] call. *)
module type S = sig
  (** [start] is called by the main thread to start broadcasting the user
      interface. Any initial setup should be performed here. *)
  val start : unit -> unit

  (** [render state] is called by the main thread to render the current state of
      the user interface. *)
  val render : state -> unit

  (** [handle_user_events ~now ~time_budget mutex] is called by the main thread
      to handle user events such as keypresses. The function should return the
      time at which the next event should be handled. A [mutex] is provided in
      order to lock the state of the UI. [time_budget] indicates an approximate
      amount of time that should be spent in this function. This is useful for
      things like waiting for user input.

      [time_budget] should be as accurate as possible, but if it's not, the only
      consequence would be modifying the rendering rate. If [time_budget] is an
      underestimation of the actual amount of time spent, we will render faster
      than the desired frame rate. If it is an overestimation, we will render
      slower. *)
  val handle_user_events : now:float -> time_budget:float -> Mutex.t -> state -> float

  (** [reset] is called by the main thread to reset the user interface. *)
  val reset : unit -> unit

  (** [reset_flush_history] is called by the main thread to reset and flush the
      user interface. *)
  val reset_flush_history : unit -> unit

  (** [finish] is called finally by the main thread to finish broadcasting the
      user interface. Any locks on the terminal should be released here. *)
  val finish : unit -> unit
end
