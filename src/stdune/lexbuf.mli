(** Lexing buffer utilities *)

type t = Lexing.lexbuf

(** Same as [Lexing.from_xxx] but also initialise the location to the beginning
    of the given file *)
val from_string : string -> fname:string -> t

val from_channel : in_channel -> fname:string -> t
