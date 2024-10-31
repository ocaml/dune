#!/usr/bin/env ocaml

open StdLabels
open Printf

let list f l = sprintf "[%s]" (String.concat ~sep:"; " (List.map l ~f))
let string s = sprintf "%S" s

let option f = function
  | None -> "None"
  | Some x -> sprintf "Some %s" (f x)
;;

let out =
  match Sys.getenv_opt "DUNE_CONFIGURE_OUTPUT" with
  | None -> "src/dune_rules/setup.ml"
  | Some out -> out
;;

let default_toggles : (string * [ `Disabled | `Enabled ]) list =
  [ "toolchains", `Enabled; "pkg_build_progress", `Disabled; "lock_dev_tool", `Disabled ]
;;

let toggles = ref default_toggles

let toggle name v =
  toggles := List.map !toggles ~f:(fun (x, y) -> x, if x = name then v else y)
;;

let toggle =
  let toggles = [ "enable", `Enabled; "disable", `Disabled ] in
  let options = List.map toggles ~f:fst in
  fun name -> Arg.Symbol (options, fun s -> List.assoc s toggles |> toggle name)
;;

let () =
  let bad fmt = ksprintf (fun s -> raise (Arg.Bad s)) fmt in
  let library_path = ref [] in
  let library_destdir = ref None in
  let mandir = ref None in
  let docdir = ref None in
  let etcdir = ref None in
  let bindir = ref None in
  let sbindir = ref None in
  let libexecdir = ref None in
  let datadir = ref None in
  let cwd = lazy (Sys.getcwd ()) in
  let dir_of_string s =
    if Filename.is_relative s then Filename.concat (Lazy.force cwd) s else s
  in
  let set_libdir s =
    let dir = dir_of_string s in
    library_path := dir :: !library_path;
    library_destdir := Some dir
  in
  let set_dir v s =
    let dir = dir_of_string s in
    v := Some dir
  in
  let args =
    [ ( "--libdir"
      , Arg.String set_libdir
      , "DIR where public libraries are looked up in the default build context. Can be \
         specified multiple times new one taking precedence. The last is used as default \
         destination directory for public libraries." )
    ; ( "--bindir"
      , Arg.String (set_dir bindir)
      , "DIR where public binaries are installed for the default build context" )
    ; ( "--sbindir"
      , Arg.String (set_dir sbindir)
      , "DIR where files for sbin section are installed for the default build context" )
    ; ( "--mandir"
      , Arg.String (set_dir mandir)
      , "DIR where man pages are installed for the default build context" )
    ; ( "--docdir"
      , Arg.String (set_dir docdir)
      , "DIR where documentation is installed for the default build context" )
    ; ( "--etcdir"
      , Arg.String (set_dir etcdir)
      , "DIR where configuration files are installed for the default build context" )
    ; ( "--libexecdir"
      , Arg.String (set_dir libexecdir)
      , "DIR where files for the libexec_root section are installed for the default \
         build context. Use --libdir if specified" )
    ; ( "--datadir"
      , Arg.String (set_dir datadir)
      , "DIR where files for the share_root section are installed for the default build \
         context" )
    ; ( "--toolchains"
      , toggle "toolchains"
      , " Enable the toolchains behaviour, allowing dune to install the compiler in a \
         user-wide directory." )
    ; ( "--pkg-build-progress"
      , toggle "pkg_build_progress"
      , " Enable the displaying of package build progress.\n\
        \      This flag is experimental and shouldn't be relied on by packagers." )
    ; ( "--lock-dev-tool"
      , toggle "lock_dev_tool"
      , " Enable ocamlformat dev-tool, allows 'dune fmt' to build ocamlformat and use \
         it, independently from the project depenedencies .\n\
        \      This flag is experimental and shouldn't be relied on by packagers." )
    ]
  in
  let anon s = bad "Don't know what to do with %s" s in
  Arg.parse (Arg.align args) anon "Usage: ocaml configure.ml [OPTIONS]\nOptions are:";
  (match !libexecdir with
   | None -> libexecdir := !library_destdir
   | Some _ -> ());
  let oc = open_out out in
  let pr fmt = fprintf oc (fmt ^^ "\n") in
  pr "let library_path = %s\n" ((list string) !library_path);
  pr "let roots : string option Install.Roots.t =";
  pr "  { lib_root = %s" (option string !library_destdir);
  pr "  ; man = %s" (option string !mandir);
  pr "  ; doc_root = %s" (option string !docdir);
  pr "  ; etc_root = %s" (option string !etcdir);
  pr "  ; share_root = %s" (option string !datadir);
  pr "  ; bin = %s" (option string !bindir);
  pr "  ; sbin = %s" (option string !sbindir);
  pr "  ; libexec_root = %s" (option string !libexecdir);
  pr "  }";
  pr "";
  List.iter !toggles ~f:(fun (name, value) ->
    pr
      "let %s = `%s"
      name
      (match value with
       | `Enabled -> "Enabled"
       | `Disabled -> "Disabled"));
  close_out oc
;;
