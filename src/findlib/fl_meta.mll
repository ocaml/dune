(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

{ open Fl_metatoken }

rule token =
  parse [ 'A'-'Z' 'a'-'z' '_' '0'-'9' '.' ]+ 
  {
    Name (Lexing.lexeme lexbuf)
  } 

  | '('
  {
    LParen
  } 

  | ')'
  { 
    RParen
  } 

  | "+="
  {
    PlusEqual
  }

  | '='
  {
    Equal
  } 

  | '-'
  {
    Minus
  }

  | ','
  {
    Comma
  } 

  | '"' [^ '"' '\\' ]* ( ( "\\\\" | "\\\"" ) [^ '"' '\\' ]* )* '"'
  {
    let s1 = Lexing.lexeme lexbuf in
    let s2 = String.sub s1 1 (String.length s1 - 2) in
    let l2 = String.length s2 in
    let rec count i n =
      if i<l2 then
	match s2.[i] with
	  '\\' -> count (i+2) (n+1)
	| _    -> count (i+1) (n+1)
      else
	n
    in
    let s3 = String.create (count 0 0) in
    let rec fill i n =
      if i<l2 then
	match s2.[i] with
	  '\\' -> s3.[n] <- s2.[i+1]; fill (i+2) (n+1)
	| c    -> s3.[n] <- c;        fill (i+1) (n+1)
      else
	()
    in
    fill 0 0;
    String s3
  } 

  | [ ' ' '\t' '\r' ]
  { 
    Space
  } 

  | '\n'
  {
    Newline
  } 

  | '#' [^ '\n']* '\n'
  {
    Newline
  } 

  | '#' [^ '\n']* eof
  {
    Eof
  } 

  | eof
  {
    Eof
  }

  | _
  {
    Unknown
  }   

{}

