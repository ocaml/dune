(* $Id: topfind.mli,v 1.3 2001/10/12 20:17:40 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* Load packages from toploops (scripts) *)

(* Note: This library is not thread-safe. Loading of packages should always
 * have occured before execution threads fork.
 *)

val predicates : string list ref
  (* The list of predicates used for package loading *)

val add_predicates : string list -> unit
  (* Adds predicates to the list of predicates *)

val syntax : string -> unit
  (* Emulates the -syntax option *)

val standard_syntax : unit -> unit
  (* Adds predicates that select the standard syntax. Same as syntax "camlp4o"
   *)

val revised_syntax : unit -> unit
  (* Adds predicates that select the revised syntax. Same as syntax "camlp4r" *)

val don't_load : string list -> unit
  (* don't_load pkglist:
   *
   * The packages named in pkglist are added to the list of packages which
   * are already loaded.
   *)

val don't_load_deeply : string list -> unit
  (* don't_load_deeply pkglist:
   *
   * The packages named in pkglist and all direct and indirect ancestors
   * are added to the list of packages which are already loaded.
   *)

val load : string list -> unit
  (* load pkglist:
   *
   * The packages from pkglist are loaded, from left to right, but packages
   * that have already been loaded are left out.
   *)

val load_deeply : string list -> unit
  (* load_deeply pkglist:
   *
   * The packages from pkglist and all direct or indirect ancestors are loaded
   * in topological order. Packages that have already been loaded are left
   * out.
   *)

val reset : unit -> unit
  (* All entries in the list of loaded packages that have been added by
   * 'load' or 'load_deeply' functions are removed from this list. This
   * means that if you execute the same 'load' or 'load_deeply' functions
   * again, the packages will be reloaded.
   *)


(* DIRECTIVES:
 *
 * This module also defines the following directives for the toploop:
 *
 * #require "<package>"
 *   loads the package (and if necessary the prerequisites of the package)
 *
 * #camlp4o
 *   loads camlp4 and selects standard syntax
 *
 * #camlp4r
 *   loads camlp4 and selects revised syntax
 *
 * #list
 *   lists the available packages (calls external command "ocamlfind")
 *)

(* ======================================================================
 * History:
 *
 * $Log: topfind.mli,v $
 * Revision 1.3  2001/10/12 20:17:40  gerd
 * 	New directive #list.
 * 	Added the real_toploop hack.
 *
 * Revision 1.2  2001/03/06 20:15:15  gerd
 * 	New functions add_predicates, standard_syntax, revised_syntax,
 * syntax.
 * 	New directives camlp4o, camlp4r.
 *
 * Revision 1.1  1999/06/26 15:45:18  gerd
 * 	Initial revision.
 *
 *
 *)
