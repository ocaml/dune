(* $Id: fl_metascanner.mli,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Fl_metatoken

val parse : in_channel -> (string * (string list * string)) list

  (* parse ch:
   * 
   * scans and parses the file connected with channel ch. The file format
   * is as following:
   *
   * - The file is a sequence of settings (not necessarily separated by
   *   linefeeds although this is recommended)
   * - A simple setting has the form 'name = "string"'
   * - A parametrized setting has the form 'name ( name, name, ... ) = "string"'
   * - Names are sequences of letters, digits, and '_'
   * - The characters quotation mark and backslash may occur within strings
   *   if they are preceded by a backslash
   * - Comments are indicated by '#'
   *
   * Parametrized settings: the arguments represent predicates. This means
   * that the setting should be used if all of the named predicates are
   * fulfilled. If more than one setting would apply, the most specific is
   * taken.
   * 
   * Return value is a list with elements (variable_name, (arguments, value))
   * where
   * - variable_name is the name to the left of '='
   * - arguments are the names within '()'. The arguments are alphabetically
   *   sorted; double arguments only occur once.
   * - value is the string on the right side
   * The list has the same order as the settings in the file that has been
   * read.
   *
   * exception Stream.Error of string:
   * - raised on syntax errors. The string explains the error.
   *)


val lookup : 
    string -> string list -> (string * (string list * string)) list -> string

  (* lookup variable_name predicate_list parsed_file:
   *
   * parsed_file is the value returned by the 'parse' function above.
   * The value for the given variable_name is looked up. The predicate_list
   * is the set of true predicates. The function returns the first value
   * for variable_name with a maximum number of predicates for which all
   * predicates are contained in predicate_list.
   *
   * exception Not_found if there is no entry in parsed_file for variable_name
   * whose predicates are all fulfilled.
   *)



(* ======================================================================
 * History:
 * 
 * $Log: fl_metascanner.mli,v $
 * Revision 1.2  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.1  2002/09/22 13:32:29  gerd
 * 	Renamed file from metascanner.mli to fl_metascanner.mli to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR metascanner.mli:
 *
 * Revision 1.2  2001/02/24 20:22:24  gerd
 * 	Corrected documentation.
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
