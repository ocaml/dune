rule token = parse
| [' ' '\t']      { token lexbuf }
| ['0'-'9']+ as i { Parser.INT (int_of_string i) }
| '+'             { Parser.PLUS }
| '-'             { Parser.MINUS }
| '*'             { Parser.TIMES }
| '/'             { Parser.DIV }
| '('             { Parser.LPAREN }
| ')'             { Parser.RPAREN }
| eof             { Parser.EOF }
| _               { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }
