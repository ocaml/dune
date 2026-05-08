let () =
  let file = Sys.argv.(1) in
  let ic = open_in file in
  let len = in_channel_length ic in
  let contents = really_input_string ic len in
  if Filename.basename file = "vdep.mli"
  then print_endline "module A = Dep_a\nmodule B = Dep_b"
  else print_string contents
