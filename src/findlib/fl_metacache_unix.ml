(* $Id: fl_metacache_unix.ml,v 1.2 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Fl_metacache;;
open Fl_split;;

(* The following requires that the Unix library is linked in *)

let list_dir d =
  let rec list fd =
    try
      let f = Unix.readdir fd in
      if f = Filename.current_dir_name || f = Filename.parent_dir_name then
	list fd
      else
	f :: list fd
    with
	End_of_file -> []
  in
  let fd = Unix.opendir d in
  try
    let l = list fd in
    Unix.closedir fd;
    l
  with
      exc -> Unix.closedir fd; raise exc
;;


let init() =
  (* Ensures that the cache is completely filled with every package
   * of the system
   *)
  let list_directory d =
    try
      list_dir d
    with
	Unix.Unix_error (code,_,_) ->
	  prerr_endline ("findlib: [WARNING] cannot read directory " ^ d ^ ": " ^
			 Unix.error_message code);
	  []
  in

  let rec run_ocamlpath path =
    match path with
      [] -> ()
    | dir :: path' ->
	let files = list_directory dir in
	List.iter
	  (fun f ->
	     (* If f/META exists: Add package f *)
	     let package_dir = Filename.concat dir f in
	     let meta_file_1 = Filename.concat package_dir "META" in
	     if Sys.file_exists meta_file_1 then begin
	       try
		 let entry = get_entry f package_dir meta_file_1 in
		 Fl_metastore.add store entry;   (* or Inconsistent_ordering *)
	       with
		   Fl_topo.Inconsistent_ordering ->
		     ()
		 | Failure s ->
		     prerr_endline ("findlib: [WARNING] " ^ s)
	     end
	     else
	       (* If f is META.pkgname: Add package pkgname *)
	       if String.length f >= 6 && String.sub f 0 5 = "META." then begin
		 let name = String.sub f 5 (String.length f - 5) in
		 try
		   let meta_file_2 = package_dir in
		   let entry = get_entry name dir meta_file_2 in
		   Fl_metastore.add store entry;   (* or Inconsistent_ordering *)
		 with
		     Fl_topo.Inconsistent_ordering ->
		       ()
		   | Failure s ->
		       prerr_endline ("findlib: [WARNING] " ^ s)
	       end;
	  )
	  files;
	run_ocamlpath path'
  in
  let add_relations() =
    let rels = ref [] in
    Fl_metastore.iter_up
      (fun entry ->
	 let r =
	   try Fl_metascanner.lookup "requires" [] entry.meta_file
	   with Not_found -> ""
	 in
	 let ancestors = Fl_split.in_words r in
	 List.iter
	   (fun p ->
	      rels := (p, entry.package_name) :: !rels
	   )
	   ancestors
      )
      store;
    List.iter
      (fun (p, p') ->
	 try
	   Fl_metastore.let_le store p p'
	 with
	     Not_found ->
               prerr_endline ("findlib: [WARNING] package " ^ p' ^ 
			      " requires package " ^ p ^ ": not found")
      )
      !rels
  in

  run_ocamlpath !ocamlpath;
  add_relations()
;;


let list_packages() =
  init();

  let l = ref [] in

  Fl_metastore.iter_up
    (fun m ->
      l := m.package_name :: !l)
    store;

  !l
;;


let users pl =
  (* Get the descendants of pl *)

  init();

  let l = ref [] in

  Fl_metastore.iter_down_at
    (fun m ->
      l := m.package_name :: !l)
    store
    pl;

  !l
;;


let module_conflict_report incpath =
  (* Find any *.cmi files occurring twice in (incpath @ package directories).
   *)
  let dir_of_module = Hashtbl.create 100 in
  let dirs = ref [] in

  let examine_dir d = 
    (* If d ends with a slash: remove it *)
    let d' = norm_dir d in
    (* If d' begins with '+': Expand *)
    let d' =
      if d' <> "" && d'.[0] = '+' then
	Filename.concat 
	  (Findlib.ocaml_stdlib()) 
	  (String.sub d' 1 (String.length d' - 1))
      else
	d'
    in

    (* Is d' new? *)
    if not (List.mem d' !dirs) then begin
      dirs := d' :: !dirs;
      (* Yes: Get all files ending in .cmi *)
      try
	let d_all = list_dir d' in   (* or Unix_error *)
	let d_cmi = List.filter 
		      (fun n -> Filename.check_suffix n ".cmi") 
		      d_all in
	(* Add the modules to dir_of_module: *)
	List.iter
	  (fun m ->
	     try
	       let entry = Hashtbl.find dir_of_module m in (* or Not_found *)
	       entry := d' :: !entry
	     with
		 Not_found ->
		   Hashtbl.add dir_of_module m (ref [d'])
	  )
	  d_cmi
      with
	Unix.Unix_error (code,_,_) ->
	  prerr_endline ("findlib: [WARNING] cannot read directory " ^ d' ^ ": " ^
			 Unix.error_message code);
    end
  in

  let print_report() =
    Hashtbl.iter
      (fun m dlist ->
	 match !dlist with
	     []
	   | [_] ->
	       ()
	   | _ ->
	       Printf.eprintf "findlib: [WARNING] Interface %s occurs in several directories: %s\n"
		 m
		 (String.concat ", " !dlist)
      )
      dir_of_module
  in

  List.iter examine_dir incpath;
  Fl_metastore.iter_up 
    (fun pkg -> examine_dir pkg.package_dir)
    store;

  print_report();
  flush stderr
;;


let remove_dups_from_path p =
  (* Removes directories which are physically the same from the path [p],
   * and returns the shortened path
   *)

  let dir_identity = Hashtbl.create 20 in

  let rec remove p =
    match p with
	d :: p' ->
	  begin try
	    let s = Unix.stat d in
	    let id = (s.Unix.st_dev, s.Unix.st_ino) in
	    if Hashtbl.mem dir_identity id then
	      remove p'
	    else begin
	      Hashtbl.add dir_identity id ();
	      d :: (remove p')
	    end
	  with error ->
	    (* Don't know anything, so the "directory" remains in the path *)
	    d :: (remove p')
	  end
      | [] ->
	  []
  in

  remove p
;;


(* ======================================================================
 * History:
 * 
 * $Log: fl_metacache_unix.ml,v $
 * Revision 1.2  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.1  2002/09/22 13:32:27  gerd
 * 	Renamed file from metacache_unix.ml to fl_metacache_unix.ml to avoid
 * name clashes
 *
 * ======================================================================
 * OLD LOGS FOR metacache_unix.ml:
 *
 * Revision 1.8  2002/06/08 14:08:55  gerd
 * 	Fix: -I +lib does no longer produce a warning.
 *
 * Revision 1.7  2002/05/20 23:19:18  gerd
 * 	Improved: The package_conflict_report does no longer complain
 * about duplicate META files if only the same directory is mentioned
 * several times.
 *
 * Revision 1.6  2002/05/16 23:48:10  gerd
 * 	Improved the warning messages.
 *
 * Revision 1.5  2002/04/23 22:43:48  gerd
 * 	Fix: Warning is now correct when a required package is missing
 *
 * Revision 1.4  2001/10/12 20:17:00  gerd
 * 	When directory names are compared, they are normalized before that.
 *
 * Revision 1.3  2001/07/24 20:00:59  gerd
 * 	Bugfix: init() initializes now the relations, too. Because of
 * this, users() works now
 *
 * Revision 1.2  2001/03/03 19:28:34  gerd
 * 	Added conflict reports.
 *
 * Revision 1.1  2001/02/24 20:21:58  gerd
 * 	Initial revision.
 *
 * 
 *)
