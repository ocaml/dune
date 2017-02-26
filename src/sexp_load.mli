open! Import

val single : string -> Sexp.Ast.t
val many   : string -> Sexp.Ast.t list
val many_or_ocaml_script : string -> Sexp_lexer.sexps_or_ocaml_script
