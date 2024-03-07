(** A backend that uses Notty to display the status line in the terminal. *)
val backend : unit -> Dune_console.Backend.t
