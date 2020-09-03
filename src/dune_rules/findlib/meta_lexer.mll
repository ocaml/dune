{
type token =
  | Name   of string
  | String of string
  | Minus
  | Lparen
  | Rparen
  | Comma
  | Equal
  | Plus_equal
  | Eof

type user_error = { user_error : 'a. Lexing.lexbuf -> string -> 'a }

let escaped_buf = Buffer.create 256
}

rule token user_error = parse
  | [' ' '\t' '\r']* { token user_error lexbuf }
  | '#' [^ '\n']* { token user_error lexbuf }
  | '\n' { Lexing.new_line lexbuf; token user_error lexbuf }

  | ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ as s { Name s }
  | '"'
      { Buffer.clear escaped_buf;
        string user_error escaped_buf lexbuf }
  | '-' { Minus }
  | '(' { Lparen }
  | ')' { Rparen }
  | ',' { Comma }
  | '=' { Equal }
  | "+=" { Plus_equal }
  | eof { Eof }
  | _ { user_error.user_error lexbuf "invalid character" }

and string user_error buf = parse
  | '"'
      { String (Buffer.contents buf) }
  | "\\\n"
  | '\n'
      { Lexing.new_line lexbuf;
        Buffer.add_char buf '\n';
        string user_error buf lexbuf }
  | '\\' (_ as c)
  | (_ as c)
      { Buffer.add_char buf c;
        string user_error buf lexbuf }
  | eof
      { user_error.user_error lexbuf "unterminated string" }
