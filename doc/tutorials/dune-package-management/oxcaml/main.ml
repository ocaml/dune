let () =
  (* The `local_` keyword requires OxCaml *)
  let local_ i = 42 in
  let j = i + 1 in
  Printf.printf "%d\n" j
