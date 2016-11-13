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
}

rule token = parse
  | [' ' '\t' '\r']* { token lexbuf }
  | '#' [^ '\n']* { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }

  | ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ as s { Name s }
  | '"' ([^ '\\' '"']* ( '\\' ['\\' '"'] [^ '\\' '"']* )* as s) '"'
      { let len = String.length s in
        let buf = Buffer.create len in
        let rec loop i =
          if i = len then
            Buffer.contents buf
          else
            match s.[i] with
            | '\\' -> Buffer.add_char s.[i + 1]; loop (i + 2)
            | _    -> Buffer.add_char s.[i    ]; loop (i + 1)
        in
        String (loop 0) }
  | '-' { Minus }
  | '(' { Lparen }
  | ')' { Rparen }
  | ',' { Comma }
  | '=' { Equal }
  | "+=" { Plus_equal }
  | eof { Eof }
  | _ { Loc.fail_lex lexbuf "invalid character" }
