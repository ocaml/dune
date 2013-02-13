(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


type token =
    Name of string
  | LParen 
  | RParen
  | Equal
  | PlusEqual
  | Minus
  | Comma
  | String of string
  | Space
  | Newline
  | Eof
  | Unknown
;;
