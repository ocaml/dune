open Import

let single fn =
  with_lexbuf_from_file fn ~f:Sexp_lexer.single

let many fn =
  with_lexbuf_from_file fn ~f:Sexp_lexer.many
