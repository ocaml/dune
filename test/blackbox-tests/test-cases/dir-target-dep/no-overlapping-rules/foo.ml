let dir = Sys.argv.(1)

let write f =
  let path = Filename.concat dir f in
  let out = open_out path in
  output_string out (f ^ " contents\n");
  close_out_noerr out

let () =
  Unix.mkdir dir 0o777;
  write "foo";
  write "bar"
