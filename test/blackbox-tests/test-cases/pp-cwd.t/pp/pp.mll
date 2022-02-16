rule main = parse
  | eof { () }
  | "_STRING_" { Printf.printf "%S" "Hello, world!"; main lexbuf }
  | _ as c { print_char c; main lexbuf }

{
  let () =
    set_binary_mode_out stdout true;
    Printf.eprintf "running preprocessor in %s\n" (Sys.getcwd ());
    let (input, pp_cwd, deps) =
      match Array.to_list Sys.argv with
      | _ :: input :: pp_cwd :: deps -> (input, pp_cwd, deps)
      | _ -> assert false
    in
    ListLabels.iter deps ~f:(fun f ->
      Printf.eprintf "dep %s exists = %b\n" f (Sys.file_exists f);
      let f = Filename.concat pp_cwd f in
      Printf.eprintf "dep %s exists = %b\n" f (Sys.file_exists f)
    );
    main (Lexing.from_channel (open_in_bin input))
}
