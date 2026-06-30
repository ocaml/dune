let process args =
  Printf.printf "Sys.argv: %s\n" (String.concat " " (Array.to_list args));
  (match Sys.getenv_opt "REFMT_PRINT_WIDTH" with
   | None -> ()
   | Some value -> Printf.printf "REFMT_PRINT_WIDTH=%s\n" value);
  Printf.printf "refmt output\n"

let () =
  match Sys.argv with
  | [| _ ; _|] -> process Sys.argv
  | _ -> assert false
