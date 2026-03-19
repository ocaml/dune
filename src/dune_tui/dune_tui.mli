(** A backend that uses Notty to display the status line in the terminal. *)
val backend : unit -> Stdune.Console.Backend.t
