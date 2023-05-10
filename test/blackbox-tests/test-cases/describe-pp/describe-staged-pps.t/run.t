  $ dune describe pp src/main.ml
  let prefixed = "my_custom_prefix_hello"
  let suffixed = "hello_my_custom_suffix"

With modules

  $ dune describe pp src/with_module/main_both.ml
  let prefixed = "my_custom_prefix_hello"
  let suffixed = "hello_my_custom_suffix"

  $ dune describe pp src/with_module/main_prefix.ml
  let prefixed = "prefixed_hello"

  $ dune describe pp src/with_module/main_suffix.ml
  let suffixed = "hello_my_custom_suffix"

With a file that does not exist
  $ dune describe pp src/with_module/wrong.ml
  Error: Could not find module corresponding to source file
  src/with_module/wrong.ml
  [1]
