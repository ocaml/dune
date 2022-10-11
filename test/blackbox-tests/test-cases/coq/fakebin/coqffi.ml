let () =
  Printf.printf "fake coqffi has run with args:\n%s\n\n"
    (String.concat "\n" (List.tl @@ Array.to_list Sys.argv));
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(3) in
  let oc = open_out output_file in
  Printf.fprintf oc "coqffi has run on %s" input_file;
  close_out oc
