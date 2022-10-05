let () =
  Printf.printf "fake coq-of-ocaml has run";
  let input_file = Sys.argv.(1) in
  let output_file = Filename.chop_extension input_file ^ ".v" in
  let oc = open_out output_file in
  (* create or truncate file, return channel *)
  Printf.fprintf oc "coq-of-ocaml has run on %s" input_file;
  close_out oc
