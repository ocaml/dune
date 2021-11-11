let () =
  match Sys.argv with
  | [| _ |] ->
    exit (Sys.command (Filename.quote_command Sys.executable_name [ "sub" ]))
  | [| _; "sub" |] ->
    let beacon_file = Sys.getenv "BEACON_FILE" in
    Printf.printf "Creating %s...\n%!" beacon_file;
    let oc = open_out beacon_file in
    Printf.fprintf oc "%d" (Unix.getpid ());
    close_out oc;
    Printf.printf "Done.\n%!";
    Unix.sleep max_int
  | _ -> assert false
