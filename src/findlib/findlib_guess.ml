(* $Id: findlib_guess.ml,v 1.3 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(* Guess the META file.
 *
 * The list of ancestors is determined by extracting the required modules from
 * the cmi files (which may be too few), and by searching the already installed
 * packages implementing these modules.
 *    TODO: Extract the required modules from cmo/cmx files.
 *)

exception Unknown_file_format;;

let imported_interfaces_ocaml3 ch =
  (* OCaml 3.XX *)
  ignore(input_value ch);         (* Skip over the list of signatures *)
  let mod_crc_list =
    (input_value ch : (string * string) list) in
  List.map fst (List.tl mod_crc_list)
;;


let imported_interfaces cmifile =
  let ch = open_in_bin cmifile in
  try
    let s = String.create 12 in
    really_input ch s 0 12;
    let mod_list =
      match s with
	| "Caml1999I006" -> imported_interfaces_ocaml3 ch  (* Ocaml 3.00 *)
	| "Caml1999I007" -> imported_interfaces_ocaml3 ch  (* Ocaml 3.01 *)
	| _              -> 
	    prerr_endline ("Unknown Caml interface format: " ^ cmifile);
	    raise Unknown_file_format
    in
    close_in ch;
    mod_list
  with
      exc -> close_in ch; raise exc
;;


let guess_required_packages excludelist cmifiles =
  let packages = Fl_package_base.list_packages() in

  let get_pkg_of_mod modname =
    List.find
      (fun pkg ->
	 let pkgdir = Findlib.package_directory pkg in
	 (* Ignore pkg, if pkgdir = stdlib *)
	 let pname = Filename.concat pkgdir "stdlib.cma" in
	 not (Sys.file_exists pname) &&
	 (let filename = String.uncapitalize modname ^ ".cmi" in
	  let fullname = Filename.concat pkgdir filename in
	  Sys.file_exists fullname)
      )
      packages
  in

  let pkg_of_mod = Hashtbl.create 100 in
  List.iter
    (fun f ->
       if Filename.check_suffix f ".cmi" then begin
	 try
	   let interfaces = imported_interfaces f in
	   List.iter
	     (fun modname ->
		if not (Hashtbl.mem pkg_of_mod modname) then begin
		  let pkg =
		    try Some(get_pkg_of_mod modname) with Not_found -> None
		  in
		  Hashtbl.add pkg_of_mod modname pkg
		end
	     )
	     interfaces
	 with
	     Unknown_file_format -> ()
       end
    )
    cmifiles;

  let required = ref [] in
  Hashtbl.iter
    (fun modname pkg ->
       match pkg with
	   Some p ->
	     if not (List.mem p !required) && not (List.mem p excludelist) then
	       required := p :: !required
	 | None ->
	     ()
    )
    pkg_of_mod;

  !required
;;


let guess_meta_file name files =
  let cma_files =
    List.filter
      (fun f ->
	 let b = Filename.basename f in
	 Filename.check_suffix b ".cma"
      )
      files
  in
  let cmxa_files =
    List.filter
      (fun f ->
	 let b = Filename.basename f in
	 Filename.check_suffix b ".cmxa"
      )
      files
  in
  let requirements = guess_required_packages [name] files in
  let requirements_str = String.concat " " requirements in

  Printf.printf "# The following META file is a guess by ocamlfind:\n";
  Printf.printf "name=\"%s\"\n" name;
  Printf.printf "version=\"[Unknown]\"\n";
  Printf.printf "description=\"bla bla\"\n";
  Printf.printf "requires=\"%s\"\n" requirements_str;
  if cma_files <> [] then begin
    let cma_files_str = String.concat " " cma_files in
    Printf.printf "archive(byte)=\"%s\"\n" cma_files_str;
    if List.length cma_files > 1 then
      Printf.printf "# order may be wrong\n";
  end;
  if cmxa_files <> [] then begin
    let cmxa_files_str = String.concat " " cmxa_files in
    Printf.printf "archive(native)=\"%s\"\n" cmxa_files_str;
    if List.length cmxa_files > 1 then
      Printf.printf "# order may be wrong\n";
  end;
  flush stdout
;;



(* ======================================================================
 * History:
 * 
 * $Log: findlib_guess.ml,v $
 * Revision 1.3  2002/09/22 20:12:32  gerd
 * 	Renamed modules (prefix fl_)
 *
 * Revision 1.2  2001/02/26 23:07:44  gerd
 * 	Improved output
 *
 * Revision 1.1  2001/02/24 20:23:54  gerd
 * 	Initial revision.
 *
 * 
 *)
