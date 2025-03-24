(** Binaries from the PATH *)

(** Character used to separate entries in [PATH] and similar environment
    variables *)
val path_sep : char

(** Parse a [PATH] like variable *)
val parse : ?sep:char -> string -> string list

(** Parse a [PATH] like variable expecting each element to be a path *)
val parse_path : ?sep:char -> string -> Path.t list

(** Return a delimited string encoding of a list of strings, such as
    is used by the [PATH] variable *)
val encode_strings : string list -> string

(** Add an entry to the contents of a [PATH] variable. *)
val cons_path : ?path_sep:char -> Path.t -> _PATH:string option -> string

(** Extension to append to executable filenames *)
val exe : string

(** Adds an [.exe] suffix unless it is already present *)
val add_exe : string -> string

(** Check if a file exists *)
val exists : Path.t -> bool

(** Look for a program in the PATH *)
val which : path:Path.t list -> string -> Path.t option
