val single : Lexing.lexbuf -> Sexp.Ast.t
val many   : Lexing.lexbuf -> Sexp.Ast.t list

type sexps_or_ocaml_script =
  | Sexps of Sexp.Ast.t list
  | Ocaml_script

val many_or_ocaml_script : Lexing.lexbuf -> sexps_or_ocaml_script

module Load : sig
  val single : string -> Sexp.Ast.t
  val many   : string -> Sexp.Ast.t list
  val many_or_ocaml_script : string -> sexps_or_ocaml_script
end
