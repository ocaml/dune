module Token = Lexer_shared.Token

type t = Lexer_shared.t

let token = Dune_lexer.token

let jbuild_token = Jbuild_lexer.token

let of_syntax = function
  | File_syntax.Dune -> token
  | Jbuild -> jbuild_token
