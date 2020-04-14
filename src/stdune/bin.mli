(** Binaries from the PATH *)

val path_sep : char
(** Character used to separate entries in [PATH] and similar environment
    variables *)

val parse_path : ?sep:char -> string -> Path.t list
(** Parse a [PATH] like variable *)

val cons_path : Path.t -> _PATH:string option -> string
(** Add an entry to the contents of a [PATH] variable. *)

val exe : string
(** Extension to append to executable filenames *)

val exists : Path.t -> bool
(** Check if a file exists *)

val which : path:Path.t list -> string -> Path.t option
(** Look for a program in the PATH *)

val make : path:Path.t list -> Path.t option
(** "make" program *)
