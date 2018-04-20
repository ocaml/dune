{
open Stdune
open Doc_ast

exception Fail of Lexing.position * string

let fail lexbuf fmt =
  Printf.ksprintf fmt (fun msg ->
    raise (Fail (Lexing.lexeme_start_p lexbuf)))
}

let spaces = [' ' '\n' '\r']
let eol = ' '* '\n' | "\r\n"
let special = ['\000'-'\0031' '*' '`']
let normal = [^'\000'-'\0031' '*' '`']
let paragraph = [^'\n']+

rule doc = parse
  | spaces* { doc lexbuf }
  | normal+ as s eol ('-'+) eol
    { S s :: doc lexbuf }
  | "**" (normal+ as s) "**" eol eol paragraph
    
