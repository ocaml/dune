let () =
  let lex = Lexing.from_string "yo" in
  ignore (Parser.main Lexer.lex lex);
