(* $Id: topfind.ml,v 1.8 2003/09/30 11:25:26 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

let predicates = ref [];;
let forbidden = ref [];;
let loaded = ref [];;
let directories = ref [];;

let real_toploop = 
  !Sys.interactive &&
  Array.length Sys.argv > 0 &&
  Filename.basename (Sys.argv.(0)) = "ocaml" ;;
(* This is a hack, and works normally for ocaml >= 3.03, because you do not
 * need other toploops than "ocaml".
 * For scripts, this should be false, because Sys.argv is shifted.
 *
 * Note: Since O'Caml 3.04, the definition 
 *   "let real_toploop = !Sys.interactive"
 * is better, because Sys.interactive is no longer true for scripts.
 *)

let add_predicates pl =
  predicates := pl @ !predicates;;

let syntax s =
  add_predicates [ "syntax"; s ];;

let standard_syntax () = syntax "camlp4o";;
let revised_syntax () = syntax "camlp4r";;


let add_dir d =
  if not (List.mem d !directories) then begin
    Topdirs.dir_directory d;
    directories := d :: !directories
  end
;;


let load pkglist =
  List.iter
    (fun pkg ->
      let stdlibdir = Findlib.ocaml_stdlib() in
      if not (List.mem pkg !loaded) then begin
        (* Determine the package directory: *)
	let d =
	  try Findlib.package_directory pkg
	  with
	    Not_found ->
	      failwith ("Topfind.load: package '" ^ pkg ^ "' not found")
	in
	add_dir d;
        (* Leave pkg out if mentioned in !forbidden *)
	if not (List.mem pkg !forbidden) then begin
	  (* Determine the 'archive' property: *)
	  let archive =
	    try Findlib.package_property !predicates pkg "archive"
	    with
	      Not_found -> ""
	  in
	  (* Split the 'archive' property and load the files: *)
	  let archives = Fl_split.in_words archive in
	  List.iter
	    (fun arch ->
	       let arch' =
		 if String.contains arch '/' then (
		   match arch.[0] with
		       '^'
		     | '+' ->
			 Filename.concat
			    stdlibdir
			    (String.sub arch 1 (String.length arch - 1))
		     | '/' ->
			 arch
		     | _ ->
			 Filename.concat d arch
		 )
		 else
		   Filename.concat d arch
	       in
	       if real_toploop then
		 prerr_endline ("Loading " ^ arch');
	       Topdirs.dir_load
		 Format.std_formatter arch')
	    archives;
	  (* The package is loaded: *)
	  loaded := pkg :: !loaded
	end
      end)
    pkglist
;;


let load_deeply pkglist =
  (* Check if packages exist: *)
  List.iter
    (fun pkg ->
      try
	let _ = Findlib.package_directory pkg in ()
      with
	Not_found ->
	  failwith ("Topfind.load_deeply: package '" ^ pkg ^ "' not found"))
    pkglist;
  (* Get the sorted list of ancestors *)
  let eff_pkglist =
    Findlib.package_deep_ancestors !predicates pkglist in
  (* Load the packages in turn: *)
  load eff_pkglist
;;


let rec remove_dups l =
  match l with
    x :: l' ->
      if List.mem x l' then remove_dups l' else x::remove_dups l'
  | [] -> []
;;


let don't_load pkglist =
  forbidden := remove_dups (pkglist @ !forbidden);
  List.iter
    (fun pkg ->
       try
	let d = Findlib.package_directory pkg in
	()
      with
	Not_found ->
	  failwith ("Topfind.don't_load: package '" ^ pkg ^ "' not found"))
    pkglist
;;


let don't_load_deeply pkglist =
  (* Check if packages exist: *)
  List.iter
    (fun pkg ->
      try
	let _ = Findlib.package_directory pkg in ()
      with
	Not_found ->
	  failwith ("Topfind.don't_load_deeply: package '" ^ pkg ^ "' not found"))
    pkglist;
  (* Get the sorted list of ancestors *)
  let eff_pkglist =
    Findlib.package_deep_ancestors !predicates pkglist in
  (* Add this to the list of forbidden packages: *)
  don't_load eff_pkglist
;;


let reset() =
  loaded := []
;;


(* Add "#require" directive: *)

Hashtbl.add
    Toploop.directive_table
    "require"
    (Toploop.Directive_string
       (fun s ->
	 try
	   load_deeply (Fl_split.in_words s)
	 with
	   Failure s ->
	     print_endline s
       ))
;;

(* Add "#camlp4o" directive: *)

Hashtbl.add
    Toploop.directive_table
    "camlp4o"
    (Toploop.Directive_none
       (fun () ->
	  try
	    standard_syntax();
	    load_deeply ["camlp4"]
	 with
	   Failure s ->
	     print_endline s
       ))
;;

(* Add "#camlp4r" directive: *)

Hashtbl.add
    Toploop.directive_table
    "camlp4r"
    (Toploop.Directive_none
       (fun () ->
	  try
	    revised_syntax();
	    load_deeply ["camlp4"]
	 with
	   Failure s ->
	     print_endline s
       ))
;;


(* Add "#list" directive: *)

Hashtbl.add
    Toploop.directive_table
    "list"
    (Toploop.Directive_none
       (fun () ->
	  try
	    ignore(Sys.command "ocamlfind list")
	 with
	   Failure s ->
	     print_endline s
       ))
;;


if real_toploop then begin
   (* Assume we are in a toploop and not a script *)
   print_endline
     ("Findlib has been successfully loaded. Additional directives:\n" ^
      "  #require \"package\";;      to load a package\n" ^
      "  #list;;                   to list the available packages\n" ^
      "  #camlp4o;;                to load camlp4 (standard syntax)\n" ^
      "  #camlp4r;;                to load camlp4 (revised syntax)\n" ^
      "  Topfind.reset();;         to force that packages will be reloaded\n")
end ;;

(* ======================================================================
 * History:
 *
 * $Log: topfind.ml,v $
 * Revision 1.8  2003/09/30 11:25:26  gerd
 * 	Generating browse_interfaces
 * 	Removed support for camltk (is now part of labltk)
 *
 * Revision 1.7  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.6  2002/04/29 13:58:30  gerd
 * 	Update: real_toploop
 *
 * Revision 1.5  2001/10/12 20:17:40  gerd
 * 	New directive #list.
 * 	Added the real_toploop hack.
 *
 * Revision 1.4  2001/03/06 20:12:54  gerd
 * 	Dropping O'Caml 2 support
 *
 * Revision 1.1  2000/04/26 00:09:20  gerd
 * 	O'Caml 3 changes.
 *
 * Revision 1.2  1999/07/10 19:51:27  gerd
 * 	Bugfix: toploops behave differently when they execute script
 * and when they are interactive toploops. In the first case the directory
 * list is cleared just before the script starts; in the latter case not.
 * To work around this, Topfind now always does Topdirs.dir_directory
 * for required packages, regardless whether they are preloaded or not.
 *
 * Revision 1.1  1999/06/26 15:45:18  gerd
 * 	Initial revision.
 *
 *
 *)
