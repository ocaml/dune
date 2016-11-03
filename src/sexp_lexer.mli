exception Parse_error of Lexing.position * string

val single : Lexing.lexbuf ->  Sexp.t * Sexp.Locs.t
val many   : Lexing.lexbuf -> (Sexp.t * Sexp.Locs.t) list
