(* $Id: fl_metacache.ml,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Fl_metascanner

type package =
    { package_name : string;
        (* the fully qualified package name, i.e. for subpackages the
	 * names of the containing packages are prepended and the name
	 * components are separated by '.'
	 *)
      package_dir : string;
      package_defs : Fl_metascanner.pkg_definition list;
    }
;;


module Fl_metaentry =
  struct
    type t = package
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


let packages_in_meta_file package_name package_dir meta_file =
  (* Parses the META file whose name is [meta_file]. In [package_name], the
   * name of the main package must be passed. [package_dir] is the
   * directory associated with the package by default (i.e. before
   * it is overriden by the "directory" directive).
   *
   * Returns the [package] records found in this file. The "directory"
   * directive is already applied.
   *)
  let rec flatten_meta pkg_name_prefix pkg_dir (pkg_name_component,pkg_expr) =
    (* Turns the recursive [pkg_expr] into a flat list of [package]s. 
     * [pkg_dir] is the default package directory. [pkg_name_prefix] is
     * the name prefix to prepend to the fully qualified package name, or
     * "". [pkg_name_component] is the local package name.
     *)
    (* Determine the final package directory: *)
    let d =
      try
	lookup "directory" [] pkg_expr.pkg_defs
      with
	  Not_found -> pkg_dir
    in
    let d' =
      if d = "" then
	pkg_dir
      else
	match d.[0] with
          | '^' 
	  | '+' -> Filename.concat
	      !ocamlstdlib
	      (String.sub d 1 (String.length d - 1))
	  | _ -> d
    in
    let p_name = 
      if pkg_name_prefix = "" then 
	pkg_name_component 
      else
	pkg_name_prefix ^ "." ^ pkg_name_component in
    let p = 
      { package_name = p_name;
	package_dir = d';
	package_defs = pkg_expr.pkg_defs
      } in
    p :: (List.flatten 
	    (List.map (flatten_meta p_name d') pkg_expr.pkg_children))
  in

  let ch = open_in meta_file in
  try
    let pkg_expr = Fl_metascanner.parse ch in
    let packages = flatten_meta "" package_dir (package_name, pkg_expr) in
    close_in ch;
    packages
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


exception No_such_package of string * string
  (* First arg is the package name not found, second arg contains additional
   * info for the user
   *)


let query package_name =
  (* Returns the [package] definition for the fully-qualified [package_name],
   * or raises [No_such_package].
   *)

  let package_name_comps = Fl_split.package_name package_name in
  if package_name_comps = [] then invalid_arg "Fl_metacache.query";
  let main_name = List.hd package_name_comps in

  let process_file_and_lookup meta_file =
    let packages = 
      packages_in_meta_file main_name package_dir meta_file in
    List.iter (Fl_metastore.add store) packages;
    try
      List.find
	(fun p -> p.package_name = package_name)
	packages
    with
	Not_found ->
	  raise (No_such_package (package_name, ""))
  in

  let rec run_ocamlpath path =
    match path with
      [] -> raise Not_found
    | dir :: path' ->
	let package_dir = Filename.concat dir main_name in
	let meta_file_1 = Filename.concat package_dir "META" in
	let meta_file_2 = Filename.concat dir ("META." ^ main_name) in
	if Sys.file_exists meta_file_1 then
	  process_file_and_lookup meta_file_1
	else
	  if Sys.file_exists meta_file_2 then
	    process_file_and_lookup meta_file_2
	  else
	    run_ocamlpath path'
  in

  try
    Fl_metastore.find store package
  with
    Not_found ->
      run_ocamlpath !ocamlpath
;;


exception Package_loop of string
  (* A package is required by itself. The arg is the name of the 
   * package 
   *)

let requires predlist package_name =
  (* returns names of packages required by [package_name], the fully qualified
   * name of the package. It is checked that the packages really exist.
   * [predlist]: list of true predicates
   * May raise [No_such_package] or [Package_loop].
   *)
  let m = query package_name in
    (* may raise No_such_package *)
  let r =
    try Fl_metascanner.lookup "requires" predlist m.package_defs
	with Not_found -> ""
  in
  let ancestors = Fl_split.in_words r in
  List.iter
    (fun p ->
      try
	let _ = query p in      (* may raise No_such_package *)
	Fl_metastore.let_le store p package_name  (* add relation *)
      with
	  No_such_package(pname,_) ->
	    raise(No_such_package(pname, "Required by `" ^ package_name ^ "'"))
	| Fl_topo.Inconsistent_ordering ->
	    raise(Package_loop p)
    )
    ancestors;
  ancestors
;;

(** TODO: Rename into Fl_package_base, add mli *)

(** STOP **)


let requires_deeply predlist package_list =
  (* returns names of packages required by the packages in [package_list],
   * either directly or indirectly.
   * It is checked that the packages really exist.
   * The list of names is sorted topologically; first comes the deepest
   * ancestor.
   * [predlist]: list of true predicates
   * - raises [Not_found] if there is no 'package'
   * - raises [Failure] if some of the ancestors do not exist
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
