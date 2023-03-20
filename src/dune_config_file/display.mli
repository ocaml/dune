(** Type of display modes.

    - [status_line] indictates if a status line is shown.
    - [verbosity] indicates how verbose the display will be. *)
type t =
  | Simple of
      { status_line : bool
      ; verbosity : Dune_engine.Display.t
      }
  | Tui

(** All the supported display modes for setting from the command line. *)
val all : (string * t) list

(** Shows a progress bar together with any errors. *)
val progress : t

(** Shows a progress bar with a verbose output showng all commands. &*)
val verbose : t

(** Shows a progress bar with a single line output for all the commands run. *)
val short : t

(** Shows only errors without a progress bar. *)
val quiet : t

(** Shows a single line output for commands run without a progress bar. Isn't
    exposed to the user, used internally for testing. *)
val short_no_status : t

val to_dyn : t -> Dyn.t

(** The console backend corresponding to the selected display mode *)
val console_backend : t -> Dune_console.Backend.t
