(* $Id: fl_metatoken.ml,v 1.1 2002/09/22 13:32:31 gerd Exp $
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
