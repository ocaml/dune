{ open Meta_parser }

rule token = parse
  | [' ' '\t']* { token lexbuf }
  | '#' [^ '\r' '\n']* { token lexbuf }
  | ("\n" | "\r\n") { Lexing.new_line lexbuf; token lexbuf }

  | ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ as s { NAME s }
  | '"' ([^'"']* as s) '"' { STRING s }
  | '-' { MINUS }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '=' { EQUAL }
  | "+=" { PLUS_EQUAL }
