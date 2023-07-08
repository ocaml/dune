(** Utilities that can't go in [Import] *)

open Import

(** Return the absolute path to the shell and the argument to pass it (-c or
    /c). Raise in case in cannot be found. *)
val system_shell_exn : ?env:Env.t -> needed_to:string -> unit -> Path.t * string

(** Raise an error about a program not found in the PATH or in the tree *)
val program_not_found :
  ?context:Context_name.t -> ?hint:string -> loc:Loc.t option -> string -> _

val program_not_found_message :
     ?context:Context_name.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> User_message.t

(** Pretty-printer for suggesting a given shell command to the user *)
val pp_command_hint : string -> _ Pp.t

val lookup_os_shell_path :
  ?env:Env.t -> ?cmd_on_windows:bool -> [ `system | `bash ] -> Path.t option
