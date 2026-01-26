let file = Sys.argv.(1)

let () =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in_noerr ch;
  print_string s
