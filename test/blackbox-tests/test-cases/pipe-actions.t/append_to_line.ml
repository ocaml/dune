let () =
  try
    while true do
      let s = read_line () in
      Printf.printf  "%s | o %s\n%!" s Sys.argv.(1);
      Printf.eprintf "%s | e %s\n%!" s Sys.argv.(1);
      ()
    done
  with End_of_file -> ()
