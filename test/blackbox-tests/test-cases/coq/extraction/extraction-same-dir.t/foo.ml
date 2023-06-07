let () =
  print_endline
    (match Extract.nb Datatypes.Coq_true with
    | Coq_true -> "true"
    | Coq_false -> "false")
