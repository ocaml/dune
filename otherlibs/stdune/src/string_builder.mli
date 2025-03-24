(** A buffer for efficiently building strings. *)
type t

(** Creates a [t] with the given capacity. *)
val create : int -> t

(** Adds a [char] to the end of the buffer. *)
val add_char : t -> char -> unit

(** Adds a [string] to the end of the buffer. *)
val add_string : t -> string -> unit

(** Adds a substring to the end of the buffer. *)
val add_substring : t -> string -> pos:int -> len:int -> unit

(** Returns the built string. Raises if the buffer is not full. Subsequent calls to
    [add_char] or [add_string] will also raise. *)
val build_exact_exn : t -> string
