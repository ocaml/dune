It should fail with a message that `describe pp` doesn't support `staged_pps`.

  $ dune describe pp src/main.ml
  let () = print_string [%add_suffix "hello"]
