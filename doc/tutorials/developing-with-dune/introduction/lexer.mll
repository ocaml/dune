let space = [' ']+

let digit = ['0'-'9']

rule token = parse
    | eof { Parser.Eof }
    | space { token lexbuf }
    | '\n' { Parser.Eof }
    | '+' { Parser.Plus }
    | digit+ { Parser.Int (int_of_string (Lexing.lexeme lexbuf)) }
