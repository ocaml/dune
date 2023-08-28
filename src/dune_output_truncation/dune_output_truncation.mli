(** Truncates the given string so to a length of approximately [n]. If the string is
    truncated, [message] is included to indicate the truncation. *)
val limit_output : string -> n:int -> message:string -> string
