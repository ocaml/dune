(** Lexing buffer utilities *)

type t = Lexing.lexbuf

val from_string : string -> fname:string -> t
(** Same as [Lexing.from_xxx] but also initialise the location to the beginning
    of the given file *)

val from_channel : in_channel -> fname:string -> t
