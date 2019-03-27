let () =
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let len = in_channel_length ic in
  let contents = really_input_string ic len in
  print_string contents
