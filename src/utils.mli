(** Utilities that can't go in [Import] *)

open Import

(** Return the absolute path to the shell, the argument to pass it (-c or /c) and a
    failure in case the shell can't be found. *)
val system_shell : needed_to:string -> Path.t * string * fail option

(** Convert a signal number to a name: INT, TERM, ... *)
val signal_name : int -> string

(** Return the path to the jbuild file in this directory as a string. *)
val jbuild_name_in : dir:Path.t -> string

(** Nice description of a target *)
val describe_target : Path.t -> string
