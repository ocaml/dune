let () =
  match Sys.argv with
  | [| _; log |] ->
    exit
      (Sys.command (Filename.quote_command Sys.executable_name [ "sub"; log ]))
  | [| _; "sub"; log |] ->
    let beacon_file = Sys.getenv "BEACON_FILE" in
    let log = open_out log in
    Printf.fprintf log "Creating %s...\n%!" beacon_file;
    let oc = open_out beacon_file in
    Printf.fprintf oc "%d" (Unix.getpid ());
    close_out oc;
    Printf.fprintf log "Done.\n%!";
    close_out log;
    Unix.sleep max_int
  | _ -> assert false
