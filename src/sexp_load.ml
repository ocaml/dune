open Import

let single fn =
  with_lexbuf_from_file fn ~f:Sexp_lexer.single

let many fn =
  with_lexbuf_from_file fn ~f:Sexp_lexer.many

let many_or_ocaml_script fn =
  with_lexbuf_from_file fn ~f:Sexp_lexer.many_or_ocaml_script
