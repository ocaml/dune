let () =
  let ic = open_in Sys.argv.(1) in
  let rec loop ic =
    match input_line ic with
    | line -> 
        print_endline line;
        loop ic
    | exception End_of_file -> ()
  in
  loop ic;
  close_in ic
