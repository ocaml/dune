#!/usr/bin/env ocaml

open StdLabels
open Printf

let list f l = sprintf "[%s]" (String.concat ~sep:"; " (List.map l ~f))

let string s = sprintf "%S" s

let option f = function
  | None -> "None"
  | Some x -> sprintf "Some %s" (f x)

let () =
  let bad fmt = ksprintf (fun s -> raise (Arg.Bad s)) fmt in
  let library_path = ref None in
  let library_destdir = ref None in
  let mandir = ref None in
  let docdir = ref None in
  let etcdir = ref None in
  let cwd = lazy (Sys.getcwd ()) in
  let dir_of_string s =
    if Filename.is_relative s then Filename.concat (Lazy.force cwd) s else s
  in
  let set_libdir s =
    let dir = dir_of_string s in
    library_path := Some [ dir ];
    library_destdir := Some dir
  in
  let set_dir v s =
    let dir = dir_of_string s in
    v := Some dir
  in
  let args =
    [ ( "--libdir"
      , Arg.String set_libdir
      , "DIR where installed libraries are for the default build context" )
    ; ( "--mandir"
      , Arg.String (set_dir mandir)
      , "DIR where man pages are installed for the default build context" )
    ; ( "--docdir"
      , Arg.String (set_dir docdir)
      , "DIR where documentation is installed for the default build context" )
    ; ( "--etcdir"
      , Arg.String (set_dir etcdir)
      , "DIR where configuration files are installed for the default build \
         context" )
    ]
  in
  let anon s = bad "Don't know what to do with %s" s in
  Arg.parse (Arg.align args) anon
    "Usage: ocaml configure.ml [OPTIONS]\nOptions are:";
  let oc = open_out "src/dune_rules/setup.ml" in
  let pr fmt = fprintf oc (fmt ^^ "\n") in
  pr "let library_path    = %s" (option (list string) !library_path);
  pr "let library_destdir = %s" (option string !library_destdir);
  pr "let mandir = %s" (option string !mandir);
  pr "let docdir = %s" (option string !docdir);
  pr "let etcdir = %s" (option string !etcdir);
  close_out oc
