(* $Id: fl_metacache.ml,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

type meta =
    { package_name : string;
      package_dir : string;
      meta_file : (string * (string list * string)) list;
    }
;;


module Fl_metaentry =
  struct
    type t = meta
    type id_t = string
    let id m = m.package_name
  end
;;


module Fl_metastore =
  Fl_topo.Make(Fl_metaentry)
;;


let ocamlpath = ref [];;
let ocamlstdlib = ref "";;

let store = Fl_metastore.create();;


let init_cache path stdlib =
  ocamlpath := path;
  ocamlstdlib := stdlib
;;


let get_entry package package_dir meta_file =
  (* Parse the META file: *)
  let ch = open_in meta_file in
  try
    let mf = Fl_metascanner.parse ch in
    let d =
      try
	Fl_metascanner.lookup "directory" [] mf
      with
	  Not_found -> package_dir
    in
    let d' =
      if d = "" then
	package_dir
      else
	match d.[0] with
          | '^' 
	  | '+' -> Filename.concat
	      !ocamlstdlib
	      (String.sub d 1 (String.length d - 1))
	  | _ -> d
    in
    
    let e =
      { package_name = package;
	package_dir = d';
	meta_file = mf;
      } in
    close_in ch;
    e
  with
      Failure s ->
	close_in ch;
	failwith ("While parsing '" ^ meta_file ^ "': " ^ s)
    | Stream.Error s ->
	close_in ch;
	failwith ("While parsing '" ^ meta_file ^ "': " ^ s)
    | any ->
	close_in ch;
	raise any
;;


let query package =
  (* returns the 'meta' entry for 'package' or raises Not_found. *)

  let rec run_ocamlpath path =
    match path with
      [] -> raise Not_found
    | dir :: path' ->
	let package_dir = Filename.concat dir package in
	let meta_file_1 = Filename.concat package_dir "META" in
	let meta_file_2 = Filename.concat dir ("META." ^ package) in
	if Sys.file_exists meta_file_1 then begin
	  let entry = get_entry package package_dir meta_file_1 in
	  Fl_metastore.add store entry;
	  entry
	end
	else
	  if Sys.file_exists meta_file_2 then begin
	    let entry = get_entry package package_dir meta_file_2 in
	    Fl_metastore.add store entry;
	    entry
	  end
	  else
	    run_ocamlpath path'
  in

  try
    Fl_metastore.find store package
  with
    Not_found ->
      run_ocamlpath !ocamlpath
;;


let requires plist package =
  (* returns names of package required by 'package'. It is checked that
   * the packages really exist.
   * 'plist': list of true predicates
   * - raises Not_found if there is no 'package'
   * - raises Failure if some of the ancestors do not exist
   *)
  let m = query package in
  let r =
    try Fl_metascanner.lookup "requires" plist m.meta_file
	with Not_found -> ""
  in
  let ancestors = Fl_split.in_words r in
  List.iter
    (fun p ->
      try
	let _ = query p in
	Fl_metastore.let_le store p package
      with
	Not_found ->
	  failwith ("Findlib: package '" ^ p ^ "' not found (required by '" ^
		    package ^ "')")
      | Fl_topo.Inconsistent_ordering ->
	  failwith ("Findlib: package '" ^ p ^ "' required by itself"))
    ancestors;
  ancestors
;;


let requires_deeply plist package_list =
  (* returns names of packages required by the packages in 'package_list',
   * either directly or indirectly.
   * It is checked that the packages really exist.
   * The list of names is sorted topologically; first comes the deepest
   * ancestor, last 'package' itself.
   * 'plist': list of true predicates
   * - raises Not_found if there is no 'package'
   * - raises Failure if some of the ancestors do not exist
   *)

  let done_pkgs = ref [] in

  let rec enter_packages pkglist =
    match pkglist with
      pkg :: pkglist' ->
	if not(List.mem pkg !done_pkgs) then begin
	  let pkg_ancestors = requires plist pkg in
	  done_pkgs := pkg :: !done_pkgs;
          enter_packages pkg_ancestors
	end;
	enter_packages pkglist'
    | [] ->
	()
  in

  enter_packages package_list;

  let l = ref [] in

  Fl_metastore.iter_up_at
    (fun m ->
      l := m.package_name :: !l)
    store
    package_list;

  List.rev !l
;;


(**********************************************************************)

(* The following two functions do not use !ocamlpath, because there may
 * be duplicates in it.
 *)

let package_definitions search_path package =
  (* Return all META files defining this [package] that occur in the 
   * directories mentioned in [search_path]
   *)
  let rec run_ocamlpath path =
    match path with
      [] -> []
    | dir :: path' ->
	let package_dir = Filename.concat dir package in
	let meta_file_1 = Filename.concat package_dir "META" in
	let meta_file_2 = Filename.concat dir ("META." ^ package) in
	if Sys.file_exists meta_file_1 then begin
	  meta_file_1 :: run_ocamlpath path'
	end
	else
	  if Sys.file_exists meta_file_2 then begin
	    meta_file_2 :: run_ocamlpath path'
	  end
	  else
	    run_ocamlpath path'
  in
  run_ocamlpath search_path
;;


let package_conflict_report search_path =
  (* search_path is usually a variant of !ocamlpath *)
  Fl_metastore.iter_up
    (fun pkg ->
       let c = package_definitions search_path pkg.package_name in
       match c with
	   [] 
	 | [_] ->
	     ()
	 | _ ->
	     Printf.eprintf "findlib: [WARNING] Package %s has multiple definitions in %s\n"
	       pkg.package_name
	       (String.concat ", " c)
    )
    store;
  flush stderr
;;

(**********************************************************************)

(* There are some more functions in Fl_metacache_unix; however these are
 * not contained in findlib.cmx?a
 *)


(* ======================================================================
 * History:
 *
 * $Log: fl_metacache.ml,v $
 * Revision 1.2  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.1  2002/09/22 13:32:25  gerd
 * 	Renamed file from metacache.ml to fl_metacache.ml to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR metacache.ml:
 *
 * Revision 1.10  2002/05/20 23:19:18  gerd
 * 	Improved: The package_conflict_report does no longer complain
 * about duplicate META files if only the same directory is mentioned
 * several times.
 *
 * Revision 1.9  2002/05/16 23:48:10  gerd
 * 	Improved the warning messages.
 *
 * Revision 1.8  2001/07/24 20:00:08  gerd
 * 	Support for dynamically set stdlib
 *
 * Revision 1.7  2001/03/03 19:28:34  gerd
 * 	Added conflict reports.
 *
 * Revision 1.6  2001/02/24 20:21:45  gerd
 * 	New function get_entry; it was previously contained in
 * the function "query".
 * 	Function "users" moved to Metacache_unix.
 *
 * Revision 1.5  2000/02/28 20:20:38  gerd
 * 	Bugfix: The recursive collection of dependency had a strange
 * bug; it did not find all ascendents.
 *
 * Revision 1.4  1999/06/26 15:01:50  gerd
 * 	Added the -descendants option.
 *
 * Revision 1.3  1999/06/24 20:17:52  gerd
 * 	Further modifications (dont know which...)
 *
 * Revision 1.2  1999/06/20 22:23:18  gerd
 * 	Works now with the core libraries.
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
