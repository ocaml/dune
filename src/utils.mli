(** Utilities that can't go in [Import] *)

open! Import

(** Return the absolute path to the shell and the argument to pass it (-c or /c). Raise in
    case in cannot be found. *)
val system_shell_exn : needed_to:string -> Path.t * string

(** Same as [system_shell_exn] but for bash *)
val bash_exn : needed_to:string -> Path.t

(** Convert a signal number to a name: INT, TERM, ... *)
val signal_name : int -> string

(** Return the path to the jbuild file in this directory as a string. *)
val jbuild_name_in : dir:Path.t -> string

(** Nice description of a target *)
val describe_target : Path.t -> string

(** Raise an error about a program not found in the PATH *)
val program_not_found : ?context:string -> string -> _
