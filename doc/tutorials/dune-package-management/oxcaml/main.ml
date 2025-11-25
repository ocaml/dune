let () =
  (* The `local_` keyword requires OxCaml *)
  let local_ i = 42 in
  let j = i + 1 in
  print_endline (Printf.sprintf "%d" j)
  