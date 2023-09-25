let () =
  match Sys.argv with
  | [| _ |] -> exit (Sys.command (Filename.quote_command Sys.executable_name [ "sub" ]))
  | [| _; "sub" |] ->
    let oc = open_out (Sys.getenv "BEACON_FILE") in
    Printf.fprintf oc "%d" (Unix.getpid ());
    close_out oc;
    Unix.sleep max_int
  | _ -> assert false
;;
