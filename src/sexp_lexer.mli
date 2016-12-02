val single : Lexing.lexbuf ->  Sexp.t * Sexp.Locs.t
val many   : Lexing.lexbuf -> (Sexp.t * Sexp.Locs.t) list
