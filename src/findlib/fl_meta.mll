(* $Id: fl_meta.mll,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

{ open Fl_metatoken }

rule token =
  parse [ 'A'-'Z' 'a'-'z' '_' '0'-'9' ]+ 
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

  | '"' [^ '"' '\\' ]* ( "\\\\" | "\\\"" [^ '"' '\\' ]* )* '"'
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

(* ======================================================================
 * History:
 * 
 * $Log: fl_meta.mll,v $
 * Revision 1.2  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.1  2002/09/22 13:32:28  gerd
 * 	Renamed file from meta.mll to fl_meta.mll to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR meta.mll:
 *
 * Revision 1.1  1999/06/20 19:26:25  gerd
 * 	Major change: Added support for META files. In META files, knowlege
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 * 
 *)
