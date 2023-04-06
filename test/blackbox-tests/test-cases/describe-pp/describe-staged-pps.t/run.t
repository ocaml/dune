It should fail with a message that `describe pp` doesn't support `staged_pps`.

  $ dune describe pp src/main_both.ml
  let prefixed = "my_custom_prefix_hello"
  let suffixed = "hello_my_custom_suffix"

  $ dune describe pp src/main_prefix.ml
  let prefixed = "prefixed_hello"

  $ dune describe pp src/main_suffix.ml
  let suffixed = "hello_my_custom_suffix"
