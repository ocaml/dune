#!/usr/bin/env ocaml

open Printf

let () =
  let bad fmt = ksprintf (fun s -> raise (Arg.Bad s)) fmt in
  let library_path = ref None in
  let set_library_path s =
    let dir =
      if Filename.is_relative s then
        Filename.concat (Sys.getcwd ()) s
      else
        s
    in
    library_path := Some dir
  in
  let args =
    [ "--library-path", Arg.String set_library_path,
      "DIR where installed libraries are for the default build context"
    ]
  in
  let anon s =
    bad "Don't know what to do with %s" s
  in
  Arg.parse (Arg.align args)
    anon "Usage: ocaml configure.ml [OPTRIONS]]\nOptions are:";
  let oc = open_out "src/setup.ml" in
  let pr fmt = fprintf oc (fmt ^^ "\n") in
  pr "let library_path = %s"
    (match !library_path with
     | None -> "None"
     | Some s -> sprintf "Some [%S]" s);
  close_out oc
