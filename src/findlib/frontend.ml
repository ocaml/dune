(* $Id: frontend.ml,v 1.45 2003/11/08 12:10:36 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

open Findlib;;

exception Usage;;

type mode =
    M_use | M_query | M_install | M_remove | M_compiler of string | M_dep
  | M_printconf | M_list | M_browser | M_call of (string*string)
  | M_doc
;;


type psubst =
    Const of string
  | Percent of char
;;


let percent_subst spec s =
  (* spec = [ 'c', [ "ctext1"; "ctext2"; ... ];
   *          'd', [ "dtext1"; "dtext2"; ... ] ]
   * All occurrences of %c in the string s are replaced as specified in spec.
   * spec is an association list with the characters of the %-notation as keys
   * and lists of strings as values. The result is a list of strings containing
   * every combination of substituted values.
   *)

  let l = String.length s in
  let rec preprocess i j =
    if j<l then begin
      match s.[j] with
	'%' ->
	  if j+1<l then begin
	    let c = s.[j+1] in
	    Const(String.sub s i (j-i)) :: Percent c :: preprocess (j+2) (j+2)
	  end
	  else failwith "bad format string"
      |	_ ->
	  preprocess i (j+1)
    end
    else
      if i<j then
	[Const(String.sub s i (j-i))]
      else
	[]
  in

  let rec subst prefix l =
    match l with
      [] -> [prefix]
    | Const s :: l' ->
	subst (prefix ^ s) l'
    | Percent c :: l' ->
	let replacements =
	  try List.assoc c spec
	  with Not_found -> failwith "bad format string" in
	List.flatten
	  (List.map
	     (fun replacement ->
	       subst (prefix ^ replacement) l')
	     replacements)
  in

  subst "" (preprocess 0 0)
;;


let rec remove_dups l =
  match l with
    x :: l' ->
      if List.mem x l' then remove_dups l' else x::remove_dups l'
  | [] -> []
;;


let arg n =
  if n < Array.length Sys.argv then Sys.argv.(n) else raise Not_found
;;


let use_package prefix pkgnames =
  (* may raise No_such_package *)
  let pdirs =
    List.map
      (fun pname ->
         "-I " ^ package_directory pname
      )
      pkgnames
  in

  print_endline (prefix ^ String.concat " " pdirs)
;;


let read_ldconf filename =
  let lines = ref [] in
  let f = open_in filename in
  try
    while true do
      let line = input_line f in
      if line <> "" then
	lines := line :: !lines
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	List.rev !lines
    | other ->
	close_in f;
	raise other
;;


let write_ldconf filename lines new_lines =
  let f = open_out filename in
  try
    List.iter
      (fun line -> output_string f (line ^ "\n"))
      (lines @ new_lines);
    close_out f;
    prerr_endline("Updated " ^ filename);
  with
      Sys_error e ->
	prerr_endline ("ocamlfind: [WARNING] Cannot write " ^ filename);
	prerr_endline ("Reason: " ^ e);
	prerr_endline ("This file contains the directories with DLLs.");
	if new_lines <> [] then begin
	  prerr_endline ("It is recommended to add the following line(s) to this file:");
	  List.iter prerr_endline new_lines
	end
;;


let is_dll p =
  let sfx = Findlib_config.dll_suffix in
  sfx <> "" && Filename.check_suffix p sfx
;;


let identify_dir d =
  let s = Unix.stat d in
  (s.Unix.st_dev, s.Unix.st_ino)
;;


let conflict_report incpath pkglist =
  (* First check whether there are several definitions for packages
   * in the current path. We remove duplicate directories first.
   * Note that all other checks are not sensitive to duplicate directories.
   *)
  Fl_package_base.package_conflict_report ~identify_dir ();

  (* Second check whether there are module conflicts *)
  let pkgpath =
    List.map Findlib.package_directory pkglist in
  Fl_package_base.module_conflict_report ~identify_dir incpath;

  (* Finally check whether there are multiple DLLs: *)
  (* Note: Only the directories mentioned in ld.conf are checked, but not the
   * directories in [incpath], and not the directories in CAML_LD_LIBRARY_PATH.
   * The idea of this check is to ensure a proper installation, and not to
   * complain about the user's special configuration.
   *)
  let ldconf = ocaml_ldconf() in
  if ldconf <> "ignore" then begin
    let dll_dirs = remove_dups (read_ldconf ldconf) in
    let dll_pairs =
      List.flatten
	(List.map
	   (fun dll_dir ->
	      let files =
		try Array.to_list (Sys.readdir dll_dir)
		with _ ->
		  prerr_endline ("ocamlfind: [WARNING] Cannot read directory " ^ 
				 dll_dir ^ " which is mentioned in ld.conf");
		  []
	      in
	      List.map
		(fun file -> (file, dll_dir))
		(List.filter is_dll files)
	   )
	   dll_dirs
	) in
    let dll_hash = Hashtbl.create 50 in
    List.iter
      (fun (file, dll_dir) -> Hashtbl.add dll_hash file dll_dir)
      dll_pairs;
    Hashtbl.iter
      (fun file dll_dir ->
	 let locations = Hashtbl.find_all dll_hash file in
	 if List.length locations > 1 then begin
	   prerr_endline ("ocamlfind: [WARNING] The DLL " ^ file ^ 
			  " occurs in multiple directories: " ^ dll_dir)
	 end
      )
      dll_hash
  end
;;


let check_package_list l =
  (* may raise No_such_package *)
  List.iter
    (fun pkg ->
       let _ = package_directory pkg in
       ()
    )
    l
;;


let run_command ?filter verbose cmd args =
  if verbose then begin
    print_string ("+ " ^ cmd ^ " " ^
		  String.concat " " args ^ "\n");
    if filter <> None then
      print_string ("  (output of this command is filtered by ocamlfind)\n")
  end;
  
  flush stdout;

  let filter_input, cmd_output =
    match filter with
	None -> Unix.stdin (* dummy *), Unix.stdout
      | Some f -> Unix.pipe() 
  in

  let pid =
    Unix.create_process
      cmd
      (Array.of_list (cmd :: args))
      Unix.stdin
      cmd_output
      Unix.stderr
  in

  begin match filter with
      Some filter_fun ->
	begin
	  Unix.close cmd_output;
	  let ch = Unix.in_channel_of_descr filter_input in
	  try
	    while true do
	      let line = input_line ch in
	      match filter_fun line with
		  None -> ()       (* Suppress line *)
		| Some line' -> print_endline line'
	    done;
	    assert false
	  with
	      End_of_file -> 
		close_in ch;
		flush stdout
	end
    | None -> ()
  end;

  let (_,status) = Unix.waitpid [] pid in
  begin
    match status with
      Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
	if verbose then
	  print_string (cmd ^ " returned with exit code " ^ string_of_int n ^ "\n");
	exit n
    | Unix.WSIGNALED _ ->
	print_string (cmd ^ " got signal and exited\n");
	exit 2
    | Unix.WSTOPPED _ ->
	failwith "Your operating system does not work correctly"
  end
;;


(************************* format expansion *************************)


let expand predicates eff_packages format =
  (* may raise No_such_package *)

    (* format:
     * %p         package name
     * %d         package directory
     * %D         description
     * %v         version
     * %a         archive file(s)
     * %A         archive files as single string
     * %o         link option(s)
     * %O         link options as single string
     *)

  List.flatten
    (List.map
       (fun pkg ->
	 let dir = package_directory pkg in
	    (* May raise No_such_package *)
	 let spec =
	   [ 'p',  [pkg];
             'd',  [dir];
	     'D',  [try package_property predicates pkg "description"
		    with Not_found -> "[n/a]"];
	     'v',  [try package_property predicates pkg "version"
	            with Not_found -> "[unspecified]"];
	     'a',  Fl_split.in_words
	             (try package_property predicates pkg "archive"
		      with Not_found -> "");
	     'A',  [String.concat " "
		       (Fl_split.in_words
		          (try package_property predicates pkg "archive"
			   with Not_found -> ""))];
	     'o',  Fl_split.in_words_ws
	             (try package_property predicates pkg "linkopts"
		      with Not_found -> "");
	     'O',  [String.concat " "
		       (Fl_split.in_words_ws
		          (try package_property predicates pkg "linkopts"
			   with Not_found -> ""))];
	   ]
	 in
	 percent_subst spec format)
       eff_packages)
;;


(************************** QUERY SUBCOMMAND ***************************)

let query_package () =
  Arg.current := 1;

  let long_format =
    "package:     %p\ndescription: %D\nversion:     %v\narchive(s):  %A\nlinkopts:    %O\nlocation:    %d\n" in
  let i_format =
    "-I %d" in
  let l_format =
    "-ccopt -L%d" in
  let a_format =
    "%a" in
  let o_format =
    "%o" in
  let p_format =
    "%p" in


  let predicates = ref [] in
  let format = ref "%d" in
  let separator = ref "\n" in
  let prefix = ref "" in
  let suffix = ref "\n" in
  let recursive = ref false in
  let descendants = ref false in

  let packages = ref [] in

  let append_predicate s =
    let pl = Fl_split.in_words s in
    predicates := !predicates @ pl
  in


  Arg.parse
    [ "-predicates", Arg.String append_predicate,
                  "      specifies comma-separated list of assumed predicates";
      "-format", Arg.String (fun s -> format := s),
              "          specifies the output format";
      "-separator", Arg.String (fun s -> separator := s),
                 "       specifies the string that separates multiple answers";
      "-prefix", Arg.String (fun s -> prefix := s),
              "          a string printed before the first answer";
      "-suffix", Arg.String (fun s -> suffix := s),
              "          a string printed after the last answer";
      "-recursive", Arg.Set recursive,
                 "       select direct and indirect ancestors/descendants, too";
      "-r", Arg.Set recursive,
         "               same as -recursive";
      "-descendants", Arg.Unit (fun () ->  descendants := true; recursive := true),
                   "     query descendants instead of ancestors; implies -recursive";
      "-d", Arg.Unit (fun () ->  descendants := true; recursive := true),
         "               same as -descendants";
      "-long-format", Arg.Unit (fun () -> format := long_format),
                   "     specifies long output format";
      "-l", Arg.Unit (fun () -> format := long_format),
         "               same as -long-format";
      "-i-format", Arg.Unit (fun () -> format := i_format),
                "        prints -I options for ocamlc";
      "-l-format", Arg.Unit (fun () -> format := l_format),
                "        prints -ccopt -L options for ocamlc";
      "-a-format", Arg.Unit (fun () -> format := a_format),
                "        prints names of archives to be linked in for ocamlc";
      "-o-format", Arg.Unit (fun () -> format := o_format),
                "        prints link options for ocamlc";
      "-p-format", Arg.Unit (fun () -> format := p_format),
                "        prints package names";
    ]
    (fun p -> packages := !packages @ [p])
"usage: ocamlfind query [ -predicates <p>  | -format <f> |
                         -long-format     | -i-format   |
                         -l-format        | -a-format   |
			 -o-format        | -p-format   |
                         -prefix <p>      | -suffix <s> |
                         -separator <s>   | 
                         -descendants     | -recursive  ] package ...";

    let eff_packages =
      if !recursive then begin
	if !descendants then
	  Fl_package_base.package_users !predicates !packages
	else
	  package_deep_ancestors !predicates !packages
      end
      else
	!packages
    in

    let answers = expand !predicates eff_packages !format in

     print_string !prefix;
     print_string (String.concat !separator answers);
     print_string !suffix;
;;


(**************** preprocessor ******************************************)

let process_pp_spec syntax_preds packages pp_opts =
  (* Returns: pp_command *)
  (* may raise No_such_package *)

  let pp_packages =
    package_deep_ancestors syntax_preds packages in
  (* the packages used for the preprocessor *)

  let preprocessor_cmds =
    List.flatten
      (List.map (fun pname ->
		   try
		     [ pname,
		       package_property syntax_preds pname "preprocessor"
		     ]
		   with
		       Not_found -> []
		)
	        pp_packages
      )
  in
    
  let preprocessor_cmd =
    if syntax_preds <> [] then
      match preprocessor_cmds with
	  [] ->
	    failwith("When using -syntax, the META variable 'preprocessor' must be set")
	| [_, cmd] -> Some cmd
	| _ ->
	    failwith("No unique value for the META variable 'preprocessor': " ^
		     String.concat ", "
		       (List.map
			  (fun (n,v) ->
			  "package " ^ n ^ " defines `" ^ v ^ "'")
			  preprocessor_cmds
		       )
		    )
    else
      None
  in

  let pp_i_options =
    List.flatten
      (List.map
	 (fun pkg ->
	    let pkgdir = package_directory pkg in
	      [ "-I"; pkgdir ]
	 )
	 pp_packages) in
  
  let pp_archives =
    if preprocessor_cmd = None then
      []
    else
      List.flatten
	(List.map
	   (fun pkg ->
	      let al = try package_property syntax_preds pkg "archive"
	               with Not_found -> "" in
	      Fl_split.in_words al
	   )
	   pp_packages) in
  
  match preprocessor_cmd with
      None -> []
    | Some cmd ->
	["-pp";
	 cmd ^ " " ^
	 String.concat " " (List.map Filename.quote pp_i_options) ^ " " ^
	 String.concat " " (List.map Filename.quote pp_archives) ^ " " ^
	 String.concat " " (List.map Filename.quote pp_opts)]
;;


(**************** OCAMLC/OCAMLMKTOP/OCAMLOPT subcommands ****************)

type pass_file_t =
    Pass of string
  | Impl of string
  | Intf of string
;;


let ocamlc which () =
  Arg.current := 1;

  let destdir = ref (default_location()) in

  let switches = ref [] in
  let pass_options = ref [] in
  let pass_files = ref [] in
  let incpath = ref [] in

  let dll_pkgs = ref [] in
  let dll_pkgs_all = ref false in

  let linkpkg = ref false in

  let packages = ref [] in
  let predicates = ref [] in
  let dontlink = ref [] in

  let syntax_preds = ref [] in
  let pp_opts = ref [] in
  let pp_specified = ref false in

  let type_of_threads = 
    try package_property [] "threads" "type_of_threads"
    with Not_found -> "ignore"
  in
  let threads_default = 
    match type_of_threads with
	"posix" -> `POSIX_threads
      | "vm"    -> `VM_threads
      | _       -> `None
  in
  let threads = ref `None in

  let add_switch name =
    Arg.Unit (fun () ->
                switches := name :: !switches;
                pass_options := !pass_options @ [name]) in
  let add_spec_fn name s =
    pass_options := !pass_options @ [name; s] in
  let add_spec name = Arg.String (add_spec_fn name) in
  let add_pkg =
    Arg.String (fun s -> packages := !packages @ (Fl_split.in_words s)) in
  let add_pred =
    Arg.String (fun s -> predicates := !predicates @ (Fl_split.in_words s)) in
  let add_dontlink =
    Arg.String (fun s -> dontlink := !dontlink @ (Fl_split.in_words s)) in
  let add_syntax_pred =
    Arg.String (fun s -> syntax_preds := !syntax_preds @ (Fl_split.in_words s)) in
  let add_pp_opt =
    Arg.String (fun s -> pp_opts := !pp_opts @ [s]) in
  let add_dll_pkg =
    Arg.String (fun s -> dll_pkgs := !dll_pkgs @ (Fl_split.in_words s)) in
  let ignore_error = ref false in


  Arg.parse
    (List.flatten
    [ [
      "-package", add_pkg,
               " <name>   Refer to package when compiling";
      "-linkpkg", Arg.Set linkpkg,
               "          Link the packages in";
      "-predicates", add_pred,
                  " <p>   Add predicate <p> when resolving package properties";
      "-dontlink", add_dontlink,
                " <name>  Do not link in package <name> and its ancestors";
      "-syntax", add_syntax_pred,
              " <p>       Use preprocessor with predicate <p>";
      "-ppopt", add_pp_opt,
             " <opt>      Append option <opt> to preprocessor invocation";
      "-dllpath-pkg", add_dll_pkg,
                   "<pkg> Add -dllpath for this package";
      "-dllpath-all", Arg.Set dll_pkgs_all,
                   "      Add -dllpath for all linked packages";
      "-ignore-error", Arg.Set ignore_error,
                    "     Ignore the 'error' directive in META files";
      "-passopt", Arg.String (fun s -> pass_options := !pass_options @ [s]),
               " <opt>    Pass option <opt> directly to ocamlc/opt/mktop\nSTANDARD OPTIONS:";

      "-a", add_switch "-a",
         "                Build a library";
      "-c", add_switch "-c",
         "                Compile only (do not link)";
      "-cc", add_spec "-cc",
          " <comp>        Use <comp> as the C compiler and linker";
      "-cclib", add_spec "-cclib",
             " <opt>      Pass option <opt> to the C linker";
      "-ccopt", add_spec "-ccopt",
             " <opt>      Pass option <opt> to the C compiler and linker";
      ];
      if which = "ocamlopt" then  [
      "-compact", add_switch "-compact",
               "          Optimize code size rather than speed"
      ]
      else [];
      if which <> "ocamlopt" then [
      "-custom", add_switch "-custom",
              "           Link in custom mode";
      "-dllib", add_spec "-dllib",
	     " <lib>      Use the dynamically-loaded library <lib>";
      ] else [];
      [
      "-dllpath", add_spec "-dllpath",
               " <dir>    Add <dir> to the run-time search path for shared libraries"];
      [
      "-dtypes", add_switch "-dtypes",
              "           Save type information in <filename>.annot";
      ];
      if which = "ocamlopt" then [
      "-ffast-math", add_switch "-ffast-math",
                  "       Inline trigonometric and exponential functions"
      ] else [];
      if which <> "ocamlopt" then [
      "-g", add_switch "-g",
         "                Save debugging information";
      ] else [];
      [
      "-i", add_switch "-i",
         "                Print the types";
      "-I", (Arg.String
	       (fun s ->
		  let s = resolve_path s in
		  incpath := s :: !incpath;  (* reverted below *)
		  add_spec_fn "-I" s )),
         " <dir>          Add <dir> to the list of include directories";
      "-impl", Arg.String (fun s -> pass_files := !pass_files @ [ Impl s ]),
            " <file>      Compile <file> as a .ml file";
      ]	;
      if which = "ocamlopt" then [
      "-inline", add_spec "-inline",
              " <n>       Set aggressiveness of inlining to <n>";
      ]	else [];
      [
      "-intf", Arg.String (fun s -> pass_files := !pass_files @ [ Intf s ]),
            " <file>      Compile <file> as a .mli file";
      "-intf-suffix", add_spec "-intf-suffix",
                   " <s>  Suffix for interface file (default: .mli)";
      "-intf_suffix", add_spec "-intf_suffix",
                   " <s>  same as -intf-suffix";
      "-labels", add_switch "-labels",
              "           Use commuting label mode";
      "-linkall", add_switch "-linkall",
               "          Link all modules, even unused ones";
      ]	;
      if which <> "ocamlopt" then [
      "-make-runtime", add_switch "-make-runtime",
                    "     Build a runtime system";
      "-make_runtime", add_switch "-make_runtime",
                    "     same as -make-runtime";
      ]	else [];
      [
      "-noautolink", add_switch "-noautolink",
                  "       Don't automatically link C libraries specif'd in .cma files";
      "-noassert", add_switch "-noassert",
                "         Do not compile assertion checks";
      "-nolabels", add_switch "-nolabels",
                "         Ignore non-optional labels in types";
      "-o", add_spec "-o",
         " <file>         Set output file name to <file>";
      "-output-obj", add_switch "-output-obj",
                  "       Output a C object file instead of an executable";
      ];
      if which = "ocamlopt" then [
      "-p", add_switch "-p",
         "                Compile/link with profiling support for \"gprof\"
                       (implies -predicates gprof)";
      ]	else if which = "ocamlcp" then [
      "-p", add_spec "-p",
	 " [afilmt]       Profile constructs specified by argument:
      a  Everything
      f  Function calls
      i  if ... then ... else
      l  while, for
      m  match ... with
      t  try ... with";
      ]	else [];
      [
      "-pack", add_switch "-pack",
            "             Package the given .cmo/.cmx files into one .cmo/.cmx";
      "-pp", Arg.String (fun s -> pp_specified := true;
			          add_spec_fn "-pp" s),
          " <command>     Pipe sources through preprocessor <command>";
      "-rectypes", add_switch "-rectypes",
                "         Allow arbitrary recursive types";
      ]	;
      if which = "ocamlopt" then [
      "-S", add_switch "-S",
         "                Keep intermediate assembly file";
      ]	 else [];
      [
      "-thread", Arg.Unit (fun _ -> threads := threads_default),
              "           Enable threads (implies -predicate mt)";
      "-unsafe", add_switch "-unsafe",
              "           No bounds checking on array and string access";
      ]	;
      if which <> "ocamlopt" then [
      "-use-runtime", add_spec "-use-runtime",
                   " <path>   Generate bytecode for the given runtime system";
      "-use_runtime", add_spec "-use_runtime",
                   "          same as -use-runtime";
      ]	 else [];
      [
      "-v", add_switch "-v",
         "                Print compiler version number";
      "-verbose", add_switch "-verbose",
               "          Print calls to external commands";
      ];
      if which <> "ocamlopt" then [
      "-vmthread", Arg.Unit (fun _ -> threads := `VM_threads),
                "         Enable threads, prefer VM threads (implies -predicate mt)";
      ] else [];
      [
      "-w", add_spec "-w",
         " <flags>        Enable or disable warnings according to <flags>:
     A/a enable/disable all warnings
     C/c enable/disable suspicious comment
     D/d enable/disable deprecated features
     E/e enable/disable fragile match
     F/f enable/disable partially applied function
     L/l enable/disable labels omitted in application
     M/m enable/disable overriden methods
     P/p enable/disable partial match
     S/s enable/disable non-unit statement
     U/u enable/disable unused match case
     V/v enable/disable hidden instance variables
     X/x enable/disable all other warnings
     default setting is Ale";
      "-warn-error", add_spec "-warn-error",
                  "       Turn these warnings into errors";
      "-where", add_switch "-where",
             "            Print standard library directory";
      "-", Arg.String (fun s -> pass_files := !pass_files @  [ Pass s ]),
	 " <file>          Treat <file> as a file name (even if it starts with `-')";
       ]
    ])
    (fun s -> pass_files := !pass_files @ [ Pass s])
    ("usage: ocamlfind " ^ which ^ " [options] file ...");

  (* ---- Start requirements analysis ---- *)

  begin match which with
    "ocamlc"     -> predicates := "byte" :: !predicates;
  | "ocamlcp"    -> predicates := "byte" :: !predicates;
  | "ocamlmktop" -> predicates := "byte" :: "toploop" :: !predicates;
  | "ocamlopt"   -> predicates := "native" :: !predicates;
  | _            -> failwith "unsupported backend"
  end;

  incpath := List.rev !incpath;

  ( match !threads with
	`None -> 
	  ()

      | `VM_threads ->
	  if which = "ocamlopt" then
	    failwith "ocamlopt does not support multi-threaded programs for your configuration";
	  pass_options := !pass_options @ [ "-vmthread" ];
	  predicates := "mt" :: "mt_vm" :: !predicates;

      | `POSIX_threads ->
	  pass_options := !pass_options @ [ "-thread" ];
	  predicates := "mt" :: "mt_posix" :: !predicates;
  );

  if List.mem "-p" !switches then
    predicates := "gprof" :: !predicates;

  if Findlib_config.ocaml_has_autolinking &&
     not (List.mem "-noautolink" !switches)
  then
    predicates := "autolink" :: !predicates;

  if !syntax_preds <> [] then begin
    predicates := "syntax" :: !predicates;
    syntax_preds := "preprocessor" :: "syntax" :: !syntax_preds;
  end;

  let verbose = List.mem "-verbose" !switches in

  if !pp_specified && !syntax_preds <> [] then
    prerr_endline("ocamlfind: [WARNING] -pp overrides the effect of -syntax partly");

  (* check packages: *)
  check_package_list !packages;
  check_package_list !dontlink;

  let eff_packages =
    package_deep_ancestors !predicates !packages in

  let eff_dontlink =
    package_deep_ancestors !predicates !dontlink in

  let eff_link =
    List.flatten
      (List.map
	 (fun pkg -> if List.mem pkg !dontlink then [] else [pkg])
	 eff_packages) in


  let eff_packages_dl =
    remove_dups (List.map package_directory eff_packages) in

  let eff_link_dl =
    remove_dups (List.map package_directory eff_link) in

  (* Conflict report: *)
  conflict_report (!incpath @ ["."; Findlib.ocaml_stdlib() ]) eff_packages;

  (* ---- End of requirements analysis ---- *)

  (* Add the pkg_<name> predicates: *)
  predicates := List.map (fun pkg -> "pkg_" ^ pkg) eff_packages @ !predicates;

  (* Check on [error] directives: *)
  List.iter
    (fun pkg ->
       try
	 let error = package_property !predicates pkg "error" in
	 if !ignore_error then
	   prerr_endline("ocamlfind: [WARNING] Package `" ^ pkg ^ 
			 "' signals error: " ^ error)
	 else
	   failwith ("Error from package `" ^ pkg ^ "': " ^ error)
       with
	   Not_found -> ()
    )
    eff_packages;

  if verbose then begin
    if !syntax_preds <> [] then
      print_string ("Effective set of preprocessor predicates: " ^
		    String.concat "," !syntax_preds ^ "\n");
    print_string ("Effective set of compiler predicates: " ^
		  String.concat "," !predicates ^ "\n");
  end;

  let stdlibdir = Fl_split.norm_dir (Findlib.ocaml_stdlib()) in
  let threads_dir = Filename.concat stdlibdir "threads" in
  let vmthreads_dir = Filename.concat stdlibdir "vmthreads" in

  let initl_file_needed =
    List.mem "toploop" !predicates && List.mem "findlib" eff_link in

  let initl_file_name =
    if initl_file_needed then
      Filename.temp_file "findlib_initl" ".ml"
    else
      ""
  in

  (* initl_file_name: the initialization code inserted at the end of
   *   the cma/cmo list (initl = init last)
   *)

  if initl_file_needed then begin
    (* Generate initializer for "findlib_top.cma" *)
    let initl = open_out_gen
		  [Open_wronly; Open_trunc; Open_text]
		  0o777
		  initl_file_name in
    try
      output_string initl
	("Topfind.don't_load [" ^
	 String.concat ";"
	   (List.map
	      (fun pkg -> "\"" ^ String.escaped pkg ^ "\"")
	      eff_link) ^
	 "];;\n");
      output_string initl
	("Topfind.predicates := [" ^
	 String.concat ";"
	   (List.map
	      (fun pred -> "\"" ^ String.escaped pred ^ "\"")
	      !predicates) ^
	 "];;\n");
      close_out initl;
    with
      any ->
	close_out initl;
	Sys.remove initl_file_name;
	raise any
  end;

  if initl_file_needed then
    at_exit
      (fun () ->
	let tr f x = try f x with _ -> () in
	tr Sys.remove initl_file_name;
	tr Sys.remove (Filename.chop_extension initl_file_name ^ ".cmi");
	tr Sys.remove (Filename.chop_extension initl_file_name ^ ".cmo");
      );

  let exclude_list = [ stdlibdir; threads_dir; vmthreads_dir ] in
  (* Don't generate -I options for these directories because there is
   * also some magic in ocamlc/ocamlopt that would not work otherwise
   *)

  let i_options =
    List.flatten
      (List.map
	 (fun pkgdir ->
	    let npkgdir = Fl_split.norm_dir pkgdir in
	    if List.mem npkgdir exclude_list then
	      []
	    else
	      [ "-I"; pkgdir;
		"-ccopt"; "-I" ^ pkgdir; ])
	 eff_packages_dl) in

  let l_options =
    List.flatten
      (List.map
	 (fun pkgdir ->
	    let npkgdir = Fl_split.norm_dir pkgdir in
	    if List.mem npkgdir exclude_list then
	      []
	    else
	      [ "-ccopt"; "-L" ^ pkgdir; ])
	 eff_link_dl) in

  let archives =
    List.flatten
      (List.map
	 (fun pkg ->
	   let al = try package_property !predicates pkg "archive"
	            with Not_found -> "" in
	   let pkg_dir =
	     if pkg = "threads" then   (* MAGIC *)
	       match !threads with
		   `None -> stdlibdir
		 | `VM_threads -> vmthreads_dir
		 | `POSIX_threads -> threads_dir
	     else
	       package_directory pkg in
	   List.map
	     (fun arch -> 
		resolve_path ~base:pkg_dir arch)
	     (Fl_split.in_words al)
	 )
	 eff_link) 
    @
    (if initl_file_needed then
       [ initl_file_name ]
     else
       []
    )
  in

  let linkopts =
    List.flatten
      (List.map
	 (fun pkg ->
	   let ol = try package_property !predicates pkg "linkopts"
	            with Not_found -> "" in
	   Fl_split.in_words_ws ol)
	 (List.rev eff_link)) in

  let pp_command = 
    if !pp_specified then
      []
    else
      process_pp_spec !syntax_preds !packages !pp_opts
  in

  let pass_files' =
    List.flatten
      (List.map
	 (function
	      Pass s ->
		if s.[0] = '-'
		then [ "-"; String.sub s 1 (String.length s - 1) ]
		else [ resolve_path s ]
	    | Impl s ->
		[ "-impl"; resolve_path s ]
	    | Intf s ->
		[ "-intf"; resolve_path s ]
	 )
	 !pass_files)
  in

  let dll_dirs =
    remove_dups
      ((List.map package_directory !dll_pkgs) @   (* XXX *)
       (if !dll_pkgs_all then eff_link_dl else [])) in

  let dll_options =
    List.flatten
      (List.map
	 (fun pkg -> ["-dllpath";  pkg] )
	 dll_dirs) in

  let arguments =
    !pass_options @    (* other options from the command line *)
    i_options @        (* Generated -I options from package analysis *)
    pp_command @       (* Optional preprocessor command *)
    (if !linkpkg then l_options else []) @  (* Generated -ccopt -L options *)
    (if !linkpkg then archives else []) @   (* Gen file names to link *)
    pass_files' @                           (* File names from cmd line *)
    (if !linkpkg then linkopts else []) @   (* Generated link options *)
    dll_options                             (* Generated -dllpath options *)
  in

  let actual_command =
    match which with
	"ocamlc"     -> Findlib.command `ocamlc
      | "ocamlopt"   -> Findlib.command `ocamlopt
      | "ocamlcp"    -> Findlib.command `ocamlcp
      | "ocamlmktop" -> Findlib.command `ocamlmktop
      | _            -> assert false
  in

  run_command verbose actual_command arguments
;;


(************************************************************************)

let ocamldoc_help = 
  "Usage: ocamlfind ocamldoc { <findlib_options> <ocamldoc_options> ... }
findlib_options are :
  -package <name>  Add this package to the search path
  -predicates <p>  Add predicate <p> when calculating dependencies
  -syntax <p>      Use preprocessor with predicate <p>
  -ppopt <opt>     Append option <opt> to preprocessor invocation
  -verbose         Be verbose
ocamldoc_options : see `ocamldoc -help'
";;


let ocamldoc() =

  let packages = ref [] in
  let predicates = ref [] in
  let syntax_preds = ref [] in
  let pp_opts = ref [] in
  let pp_specified = ref false in

  let verbose = ref false in

  let options = ref [] in

  let k = ref 2 in
  while !k < Array.length Sys.argv do
    let arg = Sys.argv.( !k ) in
    incr k;

    let next_arg() =
      if !k < Array.length Sys.argv then (
	let narg = Sys.argv.( !k ) in
	incr k;
	narg
      )
      else
	failwith ocamldoc_help
    in

    match arg with
	"-package" ->
	  packages := Fl_split.in_words (next_arg()) @ !packages;
      | "-predicates" ->
	  predicates := Fl_split.in_words (next_arg()) @ !predicates;
      | "-syntax" ->
	  syntax_preds := Fl_split.in_words (next_arg()) @ !syntax_preds;
      | "-ppopt" ->
	  pp_opts := next_arg() :: !pp_opts
      | "-verbose" 
      | "-v" ->
	  verbose := true
      | "-pp" ->
	  pp_specified := true;
	  options := !options @ [ arg ];
      | "-help" | "--help" ->
	  print_string ocamldoc_help;
	  exit 0
      | _ ->
	  options := !options @ [ arg ];
  done;

  check_package_list !packages;

  if !syntax_preds <> [] then (
    predicates := "syntax" :: !predicates;
    syntax_preds := "preprocessor" :: "syntax" :: !syntax_preds;
  );
  
  if !verbose then begin
    if !syntax_preds <> [] then
      print_string ("Effective set of preprocessor predicates: " ^
		    String.concat "," !syntax_preds ^ "\n");
    print_string ("Effective set of compiler predicates: " ^
		  String.concat "," !predicates ^ "\n");
  end;

  if !pp_specified && !syntax_preds <> [] then
    prerr_endline("Warning: -pp overrides the effect of -syntax partly");

  let pp_command = 
    if !pp_specified then
      []
    else
      process_pp_spec !syntax_preds !packages !pp_opts
  in

  let eff_packages =
    package_deep_ancestors !predicates !packages in

  let eff_packages_dl =
    remove_dups (List.map package_directory eff_packages) in

  let arguments =
    (List.flatten (List.map (fun d -> [ "-I"; d ]) eff_packages_dl)) @
    pp_command @
    !options in

  let actual_command = Findlib.command `ocamldoc in

  run_command !verbose actual_command arguments
;;


(************************************************************************)

(* From ocamldep source code: *)
let depends_on_char, continuation_char =
  match Sys.os_type with
  | "Unix" | "Win32" | "Cygwin" -> ':', '\\'
  | "MacOS" -> '\196', '\182'
  | _ -> assert false
;;


let suppress_targets suffix =
  (* If [line] begins with "target: dependencies ...", and [target] is a
   * file name ending in [suffix], this line is suppressed, and all
   * follow-up lines.
   *)
  let do_suppress = ref false in
  fun line ->
    let target =
      try
	let k = String.index_from line 0 depends_on_char in (* or Not_found *)
	let target_string = String.sub line 0 k in
	if String.contains target_string ' ' then raise Not_found;
	Some target_string
      with
	  Not_found -> None
    in
    begin match target with
	Some target_string ->
	  do_suppress := Filename.check_suffix target_string suffix;
      | None ->
	  ()
    end;
    if !do_suppress then
      None
    else
      Some line
;;


let ocamldep () =
  Arg.current := 1;

  let switches = ref [] in
  let pass_options = ref [] in
  let pass_files = ref [] in

  let packages = ref [] in
  let syntax_preds = ref [] in
  let pp_opts = ref [] in
  let pp_specified = ref false in

  let verbose = ref false in
  let native_filter = ref false in
  let bytecode_filter = ref false in

  let add_switch name =
    Arg.Unit (fun () ->
                switches := name :: !switches;
                pass_options := !pass_options @ [name]) in
  let add_spec_fn name s =
    pass_options := !pass_options @ [name; s] in
  let add_spec name = Arg.String (add_spec_fn name) in
  let add_syntax_pred =
    Arg.String (fun s -> syntax_preds := !syntax_preds @ (Fl_split.in_words s)) in
  let add_pp_opt =
    Arg.String (fun s -> pp_opts := !pp_opts @ [s]) in
  let add_pkg =
    Arg.String (fun s -> packages := !packages @ (Fl_split.in_words s)) in

  Arg.parse
      [
	"-syntax", add_syntax_pred,
                " <p>       Use preprocessor with predicate <p>";
	"-package", add_pkg,
	         " <p>      Add preprocessor package <p>";
	"-ppopt", add_pp_opt,
               " <opt>      Append option <opt> to preprocessor invocation";
	"-passopt", Arg.String (fun s -> pass_options := !pass_options @ [s]),
                 " <opt>    Pass option <opt> directly to ocamlc/opt/mktop";
	"-native-filter", Arg.Set native_filter,
	               "    Output only dependencies for native code (implies -native)";
	"-bytecode-filter", Arg.Set bytecode_filter,
	                 "  Output only dependencies for bytecode";
	"-verbose", Arg.Set verbose,
	         "          Print calls to external commands\nSTANDARD OPTIONS:";
	"-I", Arg.String (fun s -> 
			    add_spec_fn "-I" (resolve_path s)),
           " <dir>          Add <dir> to the list of include directories";
	"-native", add_switch "-native",
                "           Generate dependencies for a pure native-code project";
	"-pp", Arg.String (fun s -> pp_specified := true;
		 	            add_spec_fn "-pp" s),
            " <command>     Pipe sources through preprocessor <command>";
      ]
      (fun s -> pass_files := !pass_files @ [ s])
      ("usage: ocamlfind ocamldep [options] file ...");

  check_package_list !packages;

  if !native_filter && !bytecode_filter then
    failwith "The options -native-filter and -bytecode-filter are incompatible";
  
  if !native_filter && not (List.mem "-native" !switches) then
    pass_options := "-native" :: !pass_options;

  if !syntax_preds <> [] then
    syntax_preds := "preprocessor" :: "syntax" :: !syntax_preds;
  
  if !verbose && !syntax_preds <> [] then
    print_string ("Effective set of preprocessor predicates: " ^
		  String.concat "," !syntax_preds ^ "\n");

  if !pp_specified && !syntax_preds <> [] then
    prerr_endline("Warning: -pp overrides the effect of -syntax partly");

  let pp_command = 
    if !pp_specified then
      []
    else
      process_pp_spec !syntax_preds !packages !pp_opts
  in

  let arguments =
    !pass_options @
    pp_command @
    !pass_files
  in

  let actual_command = Findlib.command `ocamldep in
  let filter =
    if !native_filter then
      (* Suppress when target is ".cmo": *)
      Some (suppress_targets ".cmo")
    else
      if !bytecode_filter then
	(* Suppress when target is ".cmx": *)
	Some (suppress_targets ".cmx")
      else
	None
  in

  run_command ?filter !verbose actual_command arguments
;;


(************************************************************************)

let ocamlbrowser () =
  Arg.current := 1;

  let switches = ref [] in
  let pass_options = ref [] in
  let add_all = ref false in

  let packages = ref [] in

  let add_switch name =
    Arg.Unit (fun () ->
                switches := name :: !switches;
                pass_options := !pass_options @ [name]) in
  let add_spec_fn name s =
    pass_options := !pass_options @ [name; s] in
  let add_spec name = Arg.String (add_spec_fn name) in
  let add_pkg =
    Arg.String (fun s -> packages := !packages @ (Fl_split.in_words s)) in

  Arg.parse
      [
	"-I", Arg.String (fun s -> add_spec_fn "-I" (resolve_path s)),
           " <dir>          Add <dir> to the list of include directories";
	"-all", Arg.Set add_all,
	     "              Add all packages to include path";
	"-package", add_pkg,
	         " <p>      Add package <p> to include path";
	"-passopt", Arg.String (fun s -> pass_options := !pass_options @ [s]),
                 " <opt>    Pass option <opt> directly to ocamlbrowser";
      ]
      (fun s -> raise (Arg.Bad ("Unexpected argument: " ^ s)))
      ("usage: ocamlfind ocamlbrowser [options] file ...");

  if !add_all then packages := Fl_package_base.list_packages();
  check_package_list !packages;
  
  let arguments =
    !pass_options @
    (List.flatten
       (List.map
	  (fun pkg -> 
	     let dir = Findlib.package_directory pkg in
	     [ "-I"; dir ]
	  )
	  !packages
       )
    )
  in

  let actual_command = Findlib.command `ocamlbrowser in

  run_command false actual_command arguments
;;


(************************************************************************)


let copy_file ?(rename = (fun name -> name)) ?(append = "") src dstdir =
  (* A system-independent function to copy the file src to dstdir *)
  let outname = rename (Filename.basename src) in
  let ch_in = open_in_bin src in
  (* Determine the permissions of the file: the permissions of the
   * user bits are extended to all groups (user, group, world bits),
   * and the umask is applied to the result.
   * Furthermore, the mtime of the file is preserved. This seems to be
   * important for BSD-style archives (otherwise the system is confused
   * and wants that ranlib is run again). For simplicity, the atime is
   * set to the mtime, too.
   *)
  let s = Unix.stat src in
  let perm = s.Unix.st_perm in
  let user_perm = (perm land 0o700) lsr 6 in
  let perm' = user_perm lor (user_perm lsl 3) lor (user_perm lsl 6) in
  try
    let outpath = Filename.concat dstdir outname in
    if Sys.file_exists outpath then
      prerr_endline ("ocamlfind: [WARNING] Overwriting file " ^ outpath);
    let ch_out = open_out_gen 
		   [Open_wronly; Open_creat; Open_trunc; Open_binary]
		   perm'
		   outpath in
    try
      let buflen = 4096 in
      let buf = String.create buflen in
      let pos = ref 0 in
      let len = ref (input ch_in buf 0 buflen) in
      while !len > 0 do
	output ch_out buf !pos !len;
	len := input ch_in buf !pos buflen;
      done;
      output_string ch_out append;
      close_out ch_out;
      close_in ch_in;
      Unix.utimes outpath s.Unix.st_mtime s.Unix.st_mtime;

      prerr_endline("Installed " ^ outpath);
    with
	exc -> close_out ch_out; raise exc
  with
      exc -> close_in ch_in; raise exc
;;


let install_create_directory pkgname dstdir =
  try
    Unix.mkdir dstdir 0o777
  with
      Unix.Unix_error(Unix.EEXIST,_,_) ->
	failwith ("Package " ^ pkgname ^ " is already installed; please remove it first - (directory " ^ dstdir ^ " already exists)")
    | Unix.Unix_error(Unix.ENOENT,_,_)
    | Unix.Unix_error(Unix.ENOTDIR,_,_) ->
	failwith ("Bad configuration: Cannot mkdir " ^ dstdir ^ " because a path component does not exist or is not a directory")
    | Unix.Unix_error(e,_,_) ->
	failwith ("Cannot mkdir " ^ dstdir ^ ": " ^
		  Unix.error_message e)
;;


let create_owner_file pkg file =
  let outpath = file ^ ".owner" in
  let f = open_out outpath in
  try
    output_string f (pkg ^ "\n");
    close_out f;
    prerr_endline("Installed " ^ outpath);
  with
      exc -> close_out f; raise exc
;;


let find_owned_files pkg dir =
  let files = Array.to_list(Sys.readdir dir) in
  List.filter
    (fun file ->
       let owner_file = 
	 if Filename.check_suffix file ".owner" then 
	   file 
	 else 
	   file ^ ".owner" in
       (List.mem owner_file files) && (
	 let f = open_in (Filename.concat dir owner_file) in
	 try
	   let line = input_line f in
	   let is_my_file = (line = pkg) in
	   close_in f;
	   is_my_file
	 with
	     exc -> close_in f; raise exc
       )
    )
    files
;;


exception Skip_file;;

type which = Auto | Dll | No_dll;;

let install_package () =
  let destdir = ref (default_location()) in
  let metadir = ref (meta_directory()) in
  let ldconf  = ref (ocaml_ldconf()) in
  let don't_add_directory_directive = ref false in
  let pkgname = ref "" in
  let auto_files = ref [] in
  let dll_files = ref [] in
  let nodll_files = ref [] in
  let which = ref Auto in

  let keywords =
    [ "-destdir", (Arg.String (fun s -> destdir := s)),
              ("<path>    Set the destination directory (default: " ^ 
	       !destdir ^ ")");
      "-metadir", (Arg.String (fun s -> metadir := s)),
              ("<path>    Install the META file into this directory (default: "^
	       (if !metadir = "" then "none" else !metadir) ^ ")");
      "-ldconf", (Arg.String (fun s -> ldconf := s)),
             ("<path>     Update this ld.conf file (default: " ^ !ldconf ^ ")");
      "-dont-add-directory-directive", (Arg.Set don't_add_directory_directive),
                                    " never append directory='...' to META";
      "-dll", Arg.Unit (fun () -> which := Dll),
           "              The following files are DLLs";
      "-nodll", Arg.Unit (fun () -> which := No_dll),
             "            The following files are not DLLs";
    ] in
  let errmsg = "usage: ocamlfind install [options] <package_name> <file> ..." in

  Arg.current := 1;
  Arg.parse
        keywords
	(fun s ->
	   if !pkgname = ""
	   then pkgname := s
	   else 
	     match !which with
		 Auto -> auto_files := s :: !auto_files
	       | Dll  -> dll_files := s :: !dll_files
	       | No_dll -> nodll_files := s :: !nodll_files
	)
	errmsg;
  if !pkgname = "" then (Arg.usage keywords errmsg; exit 1);

  let pkgdir = Filename.concat !destdir !pkgname in
  let dlldir = Filename.concat !destdir Findlib_config.libexec_name in

  (* The list of all files to install: *)
  let full_list  = !auto_files @ !dll_files @ !nodll_files in
  (* Check whether there are DLLs: *)
  let (l1,l2)    = List.partition is_dll !auto_files in
  let dll_list   = l1 @ !dll_files in
  let nodll_list = l2 @ !nodll_files in
  let have_libexec = Sys.file_exists dlldir in

  (* Check whether META exists: *)
  let meta_dot_pkg = "META." ^ !pkgname in
  let has_meta =
    List.exists
      (fun p ->
	 let b = Filename.basename p in
	 b = "META" || b = meta_dot_pkg)
      nodll_list
  in
  if not has_meta then
    failwith "The META file is missing";

  (* Check for frequent reasons why installation can go wrong *)
  if Sys.file_exists (Filename.concat !metadir meta_dot_pkg) then
    failwith ("Package " ^ !pkgname ^ " is already installed; please remove it first - (file " ^ Filename.concat !metadir meta_dot_pkg ^ " already exists)");

  if Sys.file_exists pkgdir then
    failwith ("Package " ^ !pkgname ^ " is already installed; please remove it first - (directory " ^ pkgdir ^ " already exists)");

  if have_libexec then begin
    List.iter
      (fun dll ->
	 let b = Filename.basename dll in
	 if Sys.file_exists (Filename.concat dlldir b) then
	   failwith ("Conflict with another package: Library " ^ b ^ 
		     " has already been installed by another package");
      )
      dll_list
  end;

  (* Create the package directory: *)
  install_create_directory !pkgname pkgdir;

  (* Now copy the files into the package directory: *)
  let has_metadir = !metadir <> "" in
  List.iter
    (fun p ->
       try
	 copy_file
	   ~rename: (fun f ->
		       if has_metadir then begin
			 if f = "META" || f = meta_dot_pkg
			 then raise Skip_file
			 else f
		       end
		       else
			 if f = meta_dot_pkg then "META" else f)
	   p
	   pkgdir
       with
	   Skip_file -> ()
    )
    (if have_libexec then nodll_list else full_list);

  (* Copy META into metadir, if this has been requested *)
  if has_metadir then begin
    List.iter
      (fun p ->
	 let b = Filename.basename p in
	 if b = "META" || b = meta_dot_pkg then
	   copy_file
	     ~rename: (fun f ->
			 if f = "META" then meta_dot_pkg else f)
	     ~append: ("\ndirectory=\"" ^ pkgdir ^ "\" # auto-added by ocamlfind\n")
	     p
	     !metadir
      )
      nodll_list
  end;

  (* Copy the DLLs into the libexec directory if necessary *)
  if have_libexec then begin
    List.iter
      (fun p -> 
	 copy_file p dlldir;
	 create_owner_file !pkgname 
	   (Filename.concat dlldir (Filename.basename p))
      )
      dll_list
  end;

  (* Extend ld.conf if necessary: *)
  if dll_list <> [] && !ldconf <> "ignore" && not have_libexec then begin
    if Sys.file_exists !ldconf then
      begin
	let lines = read_ldconf !ldconf in
	write_ldconf !ldconf lines [ pkgdir ]
      end 
    else
      prerr_endline("ocamlfind: [WARNING] You have installed DLLs but there is no ld.conf")
  end;

  if dll_list <> [] && have_libexec && !ldconf <> "ignore" then begin
    (* Check whether libexec is mentioned in ldconf *)
    let lines = read_ldconf !ldconf in
    let dlldir_norm = Fl_split.norm_dir dlldir in
    if not (List.exists
	      (fun d -> Fl_split.norm_dir d = dlldir_norm)
	      lines) then
      prerr_endline("ocamlfind: [WARNING] You have installed DLLs but the directory " ^ dlldir_norm ^ " is not mentioned in ld.conf");
  end;

  (* Check if there is a postinstall script: *)
  let postinstall = Filename.concat !destdir "postinstall" in
  if Sys.file_exists postinstall then
    run_command true postinstall [ !destdir; !pkgname ]
;;


let reserved_names = [ Findlib_config.libexec_name; "postinstall"; "postremove" ];;

let remove_package () =
  let destdir = ref (default_location()) in
  let destdir_set = ref false in
  let metadir = ref (meta_directory()) in
  let ldconf  = ref (ocaml_ldconf()) in
  let pkgname = ref "" in

  let keywords =
    [ "-destdir", (Arg.String (fun s -> destdir := s; destdir_set := true)),
              ("<path>      Set the destination directory (default: " ^ 
	       !destdir ^ ")");
      "-metadir", (Arg.String (fun s -> metadir := s)),
              ("<path>      Remove the META file from this directory (default: " ^
	       (if !metadir = "" then "none" else !metadir) ^ ")");
      "-ldconf", (Arg.String (fun s -> ldconf := s)),
             ("<path>       Update this ld.conf file (default: " ^ !ldconf ^ ")");
    ] in
  let errmsg = "usage: ocamlfind remove [options] <package_name>" in

  Arg.current := 1;
  Arg.parse
        keywords
	(fun s ->
	   if !pkgname = ""
	   then pkgname := s
	   else raise (Arg.Bad "too many arguments")
	)
	errmsg;
  if !pkgname = "" then (Arg.usage keywords errmsg; exit 1);
  if List.mem !pkgname reserved_names then 
    failwith ("You are not allowed to remove this thing by ocamlfind!");

  let meta_dot_pkg = "META." ^ !pkgname in
  let has_metadir = !metadir <> "" in
  let pkgdir = Filename.concat !destdir !pkgname in
  let dlldir = Filename.concat !destdir Findlib_config.libexec_name in
  let have_libexec = Sys.file_exists dlldir in

  (* Warn if there is another package with the same name: *)
  let other_pkgdir = 
    try Findlib.package_directory !pkgname with Not_found -> "" in
  if other_pkgdir <> "" && not !destdir_set then begin
    (* Is pkgdir = other_pkgdir? - We check physical identity: *)
    try
      let s_other_pkgdir = Unix.stat other_pkgdir in
      try
	let s_pkgdir = Unix.stat pkgdir in
	if (s_pkgdir.Unix.st_dev <> s_other_pkgdir.Unix.st_dev) ||
	   (s_pkgdir.Unix.st_ino <> s_other_pkgdir.Unix.st_ino) 
	then
	  prerr_endline("ocamlfind: [WARNING] You are removing the package from " ^ pkgdir ^ " but the currently visible package is at " ^ other_pkgdir ^ "; you may want to specify the -destdir option");
      with
	  Unix.Unix_error(Unix.ENOENT,_,_) ->
	    prerr_endline("ocamlfind: [WARNING] You are trying to remove the package from " ^ pkgdir ^ " but the currently visible package is at " ^ other_pkgdir ^ "; you may want to specify the -destdir option");
    with
	Unix.Unix_error(_,_,_) -> ()    (* ignore, it's only a warning *)
  end;

  (* If there is a metadir, remove the META file from it: *)
  if has_metadir then begin
    let f = Filename.concat !metadir meta_dot_pkg in
    if Sys.file_exists f then begin
      Sys.remove f;
      prerr_endline ("Removed " ^ f);
    end
    else
      prerr_endline ("ocamlfind: [WARNING] No such file: " ^ f)
  end;

  (* Remove files from libexec directory: *)
  if have_libexec then begin
    let dll_files = find_owned_files !pkgname dlldir in
    List.iter
      (fun file ->
	 let absfile = Filename.concat dlldir file in
	 Sys.remove absfile;
	 prerr_endline ("Removed " ^ absfile)
      )
      dll_files
  end;

  (* Remove the files from the package directory: *)
  if Sys.file_exists pkgdir then begin
    let files = Sys.readdir pkgdir in
    Array.iter (fun f -> Sys.remove (Filename.concat pkgdir f)) files;
    Unix.rmdir pkgdir;
    prerr_endline ("Removed " ^ pkgdir)
  end
  else
    prerr_endline("ocamlfind: [WARNING] No such directory: " ^ pkgdir);

  (* Modify ld.conf *)
  if !ldconf <> "ignore" then begin
    if Sys.file_exists !ldconf then
      begin
	let lines = read_ldconf !ldconf in
	let d = Fl_split.norm_dir pkgdir in
	let exists = List.exists (fun p -> Fl_split.norm_dir p = d) lines in
	if exists then begin
	  let lines' = List.filter (fun p -> Fl_split.norm_dir p <> d) lines in
	  write_ldconf !ldconf lines' []
	end
      end 
  end;

  (* Check if there is a postremove script: *)
  let postremove = Filename.concat !destdir "postremove" in
  if Sys.file_exists postremove then
    run_command true postremove [ !destdir; !pkgname ]
;;


(*
let guess_meta_file () =
  let pkgname = ref "" in
  let files = ref [] in

  let keywords = [] in
  let errmsg = "usage: ocamlfind guess [options] <package_name> <file> ..." in

  Arg.current := 1;
  Arg.parse
        keywords
	(fun s ->
	   if !pkgname = ""
	   then pkgname := s
	   else files := s :: !files
	)
	errmsg;
  if !pkgname = "" then (Arg.usage keywords errmsg; exit 1);

  Findlib_guess.guess_meta_file !pkgname !files
;;
*)


let list_packages() =

  let descr = ref false in

  let keywords = 
    [ "-describe", Arg.Set descr,
                "    Output package descriptions";
    ] in
  let errmsg = "usage: ocamlfind list [options]" in

  Arg.current := 1;
  Arg.parse 
      keywords 
      (fun _ -> Arg.usage keywords errmsg; exit 1)
      errmsg;
  
  Findlib.list_packages ~descr:!descr stdout;
  Fl_package_base.package_conflict_report ~identify_dir ()
;;


let print_configuration() =
  let dir s =
    if Sys.file_exists s then
      s
    else
      s ^ " (not found)"
  in

  let var = ref None in
  let errmsg = "usage: ocamlfind printconf (conf|path|destdir|metadir|stdlib|ldconf)" in

  Arg.current := 1;
  Arg.parse
        []
	(fun s ->
	   if !var <> None then raise(Arg.Bad "Unexpected argument");
	   match s with
	       ("conf" | "path" | "destdir" | "metadir" | "stdlib" | "ldconf") ->
		 var := Some s
	     | _ ->
		 raise(Arg.Bad "Bad argument");
	)
	errmsg;

  match !var with
      None ->
	print_endline "Effective configuration:";
	Printf.printf "Configuration file:\n    %s\n"
	  (dir Findlib_config.config_file);
	Printf.printf "Search path:\n";
	List.iter
	  (fun p -> Printf.printf "    %s\n" (dir p))
	  (Findlib.search_path());
	Printf.printf "Packages will be installed in/removed from:\n    %s\n"
	  (dir (Findlib.default_location()));
	Printf.printf "META files will be installed in/removed from:\n    %s\n"
	  (let md = Findlib.meta_directory() in
	   if md = "" then "the corresponding package directories" else dir md
	  );
	Printf.printf "The standard library is assumed to reside in:\n    %s\n"
	  (Findlib.ocaml_stdlib());
	Printf.printf "The ld.conf file can be found here:\n    %s\n"
	  (Findlib.ocaml_ldconf());
	flush stdout
    | Some "conf" ->
	print_endline Findlib_config.config_file
    | Some "path" ->
	List.iter print_endline (Findlib.search_path())
    | Some "destdir" ->
	print_endline (Findlib.default_location())
    | Some "metadir" ->
	print_endline (Findlib.meta_directory())
    | Some "stdlib" ->
	print_endline (Findlib.ocaml_stdlib())
    | Some "ldconf" ->
	print_endline (Findlib.ocaml_ldconf())
    | _ ->
	assert false
;;


let ocamlcall pkg cmd =
  let dir = package_directory pkg in
  let path = Filename.concat dir cmd in
  begin
    try Unix.access path [ Unix.X_OK ]
    with
	Unix.Unix_error (Unix.ENOENT, _, _) ->
	  failwith ("Cannot find command: " ^ path)
      | Unix.Unix_error (Unix.EACCES, _, _) ->
	  failwith ("Cannot execute: " ^ path)
      | other ->
	  Unix.handle_unix_error (fun () -> raise other) ()
  end;
  let args = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv -2)) in
  run_command false path args
;;


let select_mode() =
  let m_string = try arg 1 with Not_found -> raise Usage in
  let m =
    match m_string with
      ("use"|"-use")                       -> M_use
    | ("query"|"-query")                   -> M_query
    | ("install"|"-install")               -> M_install
    | ("remove"|"-remove")                 -> M_remove
    | ("ocamlc"|"-ocamlc"|"c")             -> M_compiler "ocamlc"
    | ("ocamlcp"|"-ocamlcp"|"cp")          -> M_compiler "ocamlcp"
    | ("ocamlmktop"|"-ocamlmktop"|"mktop") -> M_compiler "ocamlmktop"
    | ("ocamlopt"|"-ocamlopt"|"opt")       -> M_compiler "ocamlopt"
    | ("ocamldep"|"-ocamldep"|"dep")       -> M_dep 
    | ("ocamlbrowser"|"-ocamlbrowser"|"browser") -> M_browser
    | ("ocamldoc"|"-ocamldoc"|"doc")       -> M_doc
    | ("printconf"|"-printconf")           -> M_printconf
    | ("list"|"-list")                     -> M_list
    | s when String.contains m_string '/' -> 
	let k = String.index m_string '/' in
	let pkg = String.sub m_string 0 k in
	let cmd = String.sub m_string (k+1) (String.length m_string - k - 1) in
	M_call(pkg,cmd)
    | _ -> raise Usage
  in

  m
;;


let main() =
  try
    let m = select_mode() in
    let l = Array.length Sys.argv in
    let rest = Array.sub Sys.argv 2 (l-2) in
    match m with
      M_use            -> if rest = [| |] then raise Usage;
                          if rest.(0) = "-p" then begin
	                    if l<4 then raise Usage;
	                      use_package rest.(1)
	                          (List.tl(List.tl(Array.to_list rest)))
	                  end
	                  else
                            use_package "" (Array.to_list rest)
    | M_query          -> query_package ()
    | M_install        -> install_package()
    | M_remove         -> remove_package ()
    | M_printconf      -> print_configuration ()
    | M_list           -> list_packages()
    | M_compiler which -> ocamlc which ()
    | M_dep            -> ocamldep()    
    | M_browser        -> ocamlbrowser()
    | M_doc            -> ocamldoc()
    | M_call(pkg,cmd)  -> ocamlcall pkg cmd
  with
    Usage ->
      prerr_endline "usage: ocamlfind query        [-help | other options] <package_name> ...";
      prerr_endline "   or: ocamlfind ocamlc       [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind ocamlcp      [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind ocamlmktop   [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind ocamlopt     [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind ocamldep     [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind ocamlbrowser [-help | other options]";
      prerr_endline "   or: ocamlfind ocamldoc     [-help | other options] <file> ...";
      prerr_endline "   or: ocamlfind install      [-help | other options] <package_name> <file> ...";
      prerr_endline "   or: ocamlfind remove       [-help | other options] <package_name>";
      prerr_endline "   or: ocamlfind printconf    [-help] [variable]";
      prerr_endline "   or: ocamlfind list";
      prerr_endline "   or: ocamlfind pkg/cmd arg ...";
      prerr_endline "abbreviations: e.g. ocamlfind opt instead of ocamlfind ocamlopt";
      exit 2
  | Failure f ->
      prerr_endline ("ocamlfind: " ^ f);
      exit 2
  | Sys_error f ->
      prerr_endline ("ocamlfind: " ^ f);
      exit 2
  | Findlib.No_such_package(pkg,info) ->
      prerr_endline ("ocamlfind: Package `" ^ pkg ^ "' not found" ^
		     (if info <> "" then " - " ^ info else ""));
      exit 2
  | Findlib.Package_loop pkg ->
      prerr_endline ("ocamlfind: Package `" ^ pkg ^ "' requires itself");
      exit 2
;;


try
  Sys.catch_break true;
  main()
with
  any ->
    prerr_endline ("Uncaught exception: " ^ Printexc.to_string any);
    let raise_again =
      try ignore(Sys.getenv "OCAMLFIND_DEBUG"); true
      with Not_found -> false
    in
    if raise_again then raise any;
    exit 3
;;
