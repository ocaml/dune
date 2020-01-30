(** Unix Signal helpers *)

(** Convert a signal number to a name: "INT", "TERM", ... *)
val name : int -> string

(** Signal used to indicate that the terminal was resized *)
val sigwinch : int option
