(** Binaries from the PATH *)

(** Character used to separate entries in [PATH] and similar environment
    variables *)
val path_sep : char

(** Parse a [PATH] like variable *)
val parse_path : ?sep:char -> string -> Path.t list

(** Add an entry to the contents of a [PATH] variable. *)
val cons_path : Path.t -> _PATH:string option -> string

(** Extension to append to executable filenames *)
val exe : string

(** Check if a file exists *)
val exists : Path.t -> bool

(** Look for a program in the PATH *)
val which : path:Path.t list -> string -> Path.t option

(** "make" program *)
val make : path:Path.t list -> Path.t option
