(* $Id: fl_metatoken.ml,v 1.1 2002/09/22 13:32:31 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


type token =
    Name of string
  | LParen 
  | RParen
  | Equal
  | Comma
  | String of string
  | Space
  | Newline
  | Eof
  | Unknown
;;


(* ======================================================================
 * History:
 * 
 * $Log: fl_metatoken.ml,v $
 * Revision 1.1  2002/09/22 13:32:31  gerd
 * 	Renamed file from metatoken.ml to fl_metatoken.ml to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR metatoken.ml:
 *
 * Revision 1.1  1999/06/20 19:26:26  gerd
 * 	Major change: Added support for META files. In META files, knowlege
 * about compilation options, and dependencies on other packages can be stored.
 * The "ocamlfind query" subcommand has been extended in order to have a
 * direct interface for that. "ocamlfind ocamlc/ocamlopt/ocamlmktop/ocamlcp"
 * subcommands have been added to simplify the invocation of the compiler.
 *
 * 
 *)
