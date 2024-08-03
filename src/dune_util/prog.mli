open Import

(** Return the absolute path to the shell and the argument to pass it (-c or
    /c). Raise in case in cannot be found. *)
val system_shell_exn : needed_to:string -> Path.t * string
