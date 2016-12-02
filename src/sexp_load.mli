open! Import

val single : string -> (Sexp.t -> 'a) -> 'a
val many   : string -> (Sexp.t -> 'a) -> 'a list
