(* $Id: findlib.mlp,v 1.11 2002/09/22 20:12:32 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

exception No_such_package 
  = Fl_package_base.No_such_package


exception Package_loop 
  = Fl_package_base.Package_loop




let conf_default_location = ref "";;
let conf_meta_directory = ref "";;
let conf_search_path = ref [];;
let conf_command = ref [];;
let conf_stdlib = ref "";;
let conf_ldconf = ref "";;

let ocamlc_default = "ocamlc";;
let ocamlopt_default = "ocamlopt";;
let ocamlcp_default = "ocamlcp";;
let ocamlmktop_default = "ocamlmktop";;
let ocamldep_default = "ocamldep";;
let ocamlbrowser_default = "ocamlbrowser";;
let ocamldoc_default = "ocamldoc";;


let init_manually 
      ?(ocamlc_command = ocamlc_default)
      ?(ocamlopt_command = ocamlopt_default)
      ?(ocamlcp_command = ocamlcp_default)
      ?(ocamlmktop_command = ocamlmktop_default)
      ?(ocamldep_command = ocamldep_default)
      ?(ocamlbrowser_command = ocamlbrowser_default)
      ?(ocamldoc_command = ocamldoc_default)
      ?(stdlib = Findlib_config.ocaml_stdlib)
      ?(ldconf = Findlib_config.ocaml_ldconf)
      ~install_dir
      ~meta_dir
      ~search_path () =
  conf_command := [ `ocamlc,     ocamlc_command;
		    `ocamlopt,   ocamlopt_command;
		    `ocamlcp,    ocamlcp_command;
		    `ocamlmktop, ocamlmktop_command;
		    `ocamldep,   ocamldep_command;
		    `ocamlbrowser, ocamlbrowser_command;
		    `ocamldoc,   ocamldoc_command;
		  ];
  conf_search_path := search_path;
  conf_default_location := install_dir;
  conf_meta_directory := meta_dir;
  conf_stdlib := stdlib;
  conf_ldconf := ldconf;
  Fl_package_base.init !conf_search_path stdlib
;;


let command_names cmd_spec =
  try
    let cmd_list = Fl_split.in_words cmd_spec in
    List.map
      (fun cmd_setting ->
	 try
	   (* cmd_setting: formal_name=actual_name *)
	   let l = String.length cmd_setting in
	   let n = String.index cmd_setting '=' in
	   let cmd_formal_name = String.sub cmd_setting 0 n in
	   let cmd_actual_name = String.sub cmd_setting (n+1) (l-n-1) in
	   cmd_formal_name, cmd_actual_name
	 with
	     Not_found ->
	       prerr_endline ("Warning: Please check the environment variable OCAMLFIND_COMMANDS");
	       "", ""
      )
      cmd_list
  with
      Not_found ->
	[]
;;

let init
      ?env_ocamlpath ?env_ocamlfind_destdir ?env_ocamlfind_metadir
      ?env_ocamlfind_commands ?env_camllib ?env_ldconf
      ?config () =
  
  let config_file =
    match config with
	Some f -> f
      | None ->
	  let p =
	    ( try Sys.getenv "OCAMLFIND_CONF" with Not_found -> "") in
	  if p = "" then Findlib_config.config_file else p
  in

  let sys_ocamlc, sys_ocamlopt, sys_ocamlcp, sys_ocamlmktop, sys_ocamldep,
      sys_ocamlbrowser, sys_ocamldoc,
      sys_search_path, sys_destdir, sys_metadir, sys_stdlib, sys_ldconf = begin
    if Sys.file_exists config_file then begin
      let ch = open_in config_file in
      try
	let vars = (Fl_metascanner.parse ch).Fl_metascanner.pkg_defs in
	let lookup name default =
	  try Fl_metascanner.lookup name [] vars
	  with Not_found -> default
	in
	let tuple =
	  ( (lookup "ocamlc" ocamlc_default),
	    (lookup "ocamlopt" ocamlopt_default),
	    (lookup "ocamlcp" ocamlcp_default),
	    (lookup "ocamlmktop" ocamlmktop_default),
	    (lookup "ocamldep" ocamldep_default),
	    (lookup "ocamlbrowser" ocamlbrowser_default),
	    (lookup "ocamldoc" ocamldoc_default),
	    Fl_split.path (lookup "path" ""),
	    (lookup "destdir" ""),
	    (lookup "metadir" "none"),
	    (lookup "stdlib" Findlib_config.ocaml_stdlib),
	    (lookup "ldconf" Findlib_config.ocaml_ldconf)
	  )
	in
	close_in ch;
	tuple
      with
	  exc -> 
	    close_in ch; raise exc
    end
    else
      ( ocamlc_default, ocamlopt_default, ocamlcp_default, ocamlmktop_default,
	ocamldep_default, ocamlbrowser_default, ocamldoc_default,
	[],
	"",
        "none",
	Findlib_config.ocaml_stdlib,
	Findlib_config.ocaml_ldconf
      )
  end
  in

  let env_commands = 
    match env_ocamlfind_commands with
	Some x -> command_names x
      | None   -> command_names (try Sys.getenv "OCAMLFIND_COMMANDS"
				 with Not_found -> "")
  in
  let env_destdir = 
    match env_ocamlfind_destdir with
	Some x -> x
      | None   ->
	  try Sys.getenv "OCAMLFIND_DESTDIR" with Not_found -> "" 
  in
  let env_metadir = 
    match env_ocamlfind_metadir with
	Some x -> x
      | None   ->
	  try Sys.getenv "OCAMLFIND_METADIR" with Not_found -> "" 
  in
  let env_search_path = 
    Fl_split.path
      (match env_ocamlpath with
	   Some x -> x
	 | None -> 
	     try Sys.getenv "OCAMLPATH" with Not_found -> "" 
      )
  in
  let env_stdlib =
    match env_camllib with
	Some x -> x
      | None ->
	  ( try Sys.getenv "OCAMLLIB"
	    with 
		Not_found ->
		  (try Sys.getenv "CAMLLIB" with Not_found -> "" )
	  )
  in
  let env_ldconf =
    match env_ldconf with
	Some x -> x
      | None ->
	  try Sys.getenv "OCAMLFIND_LDCONF" with Not_found -> ""
  in

  let ocamlc, ocamlopt, ocamlcp, ocamlmktop, ocamldep, ocamlbrowser,
      ocamldoc,
      search_path, destdir, metadir, stdlib, ldconf =
    (try List.assoc "ocamlc"     env_commands with Not_found -> sys_ocamlc),
    (try List.assoc "ocamlopt"   env_commands with Not_found -> sys_ocamlopt),
    (try List.assoc "ocamlcp"    env_commands with Not_found -> sys_ocamlcp),
    (try List.assoc "ocamlmktop" env_commands with Not_found -> sys_ocamlmktop),
    (try List.assoc "ocamldep"   env_commands with Not_found -> sys_ocamldep),
    (try List.assoc "ocamlbrowser" env_commands with Not_found -> sys_ocamlbrowser),
    (try List.assoc "ocamldoc"   env_commands with Not_found -> sys_ocamldoc),
    (env_search_path @ sys_search_path),
    (if env_destdir = "" then sys_destdir else env_destdir),
    (if env_metadir = "" then sys_metadir else env_metadir),
    (if env_stdlib  = "" then sys_stdlib  else env_stdlib),
    (if env_ldconf  = "" then sys_ldconf  else env_ldconf)
  in

  init_manually
    ~ocamlc_command: ocamlc
    ~ocamlopt_command: ocamlopt
    ~ocamlcp_command: ocamlcp
    ~ocamlmktop_command: ocamlmktop
    ~ocamldep_command: ocamldep
    ~ocamlbrowser_command: ocamlbrowser
    ~ocamldoc_command: ocamldoc
    ~stdlib: stdlib
    ~ldconf: ldconf
    ~install_dir: destdir
    ~meta_dir: metadir
    ~search_path: search_path
    ()
;;


let default_location() = !conf_default_location;;


let meta_directory() =
  if !conf_meta_directory = "none" then "" else !conf_meta_directory;;


let search_path() = !conf_search_path;;


let command which =
  try 
    List.assoc which !conf_command
  with
      Not_found -> assert false
;;


let ocaml_stdlib() = !conf_stdlib;;


let ocaml_ldconf() = !conf_ldconf;;


let package_directory pkg =
  (Fl_package_base.query pkg).Fl_package_base.package_dir
;;


let package_property predlist pkg propname =
  let l = Fl_package_base.query pkg in
  Fl_metascanner.lookup propname predlist l.Fl_package_base.package_defs
;;


let package_ancestors predlist pkg =
  Fl_package_base.requires predlist pkg
;;


let package_deep_ancestors predlist pkglist =
  Fl_package_base.requires_deeply predlist pkglist
;;


init();
