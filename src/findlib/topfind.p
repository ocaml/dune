(* $Id: topfind.p,v 1.1 2002/04/26 14:51:37 gerd Exp $ -*- tuareg -*- *)

(* For Ocaml-3.03 and up, so you can do: #use "topfind" and get a
 * working findlib toploop.
 *)
#load "@SITELIB@/findlib/findlib.cma";;
#load "@SITELIB@/findlib/findlib_top.cma";;
#directory "@SITELIB@/findlib";;
Topfind.add_predicates [ "byte"; "toploop" ];
Topfind.don't_load ["findlib"];;
