When there are explicit interfaces, modules must be rebuilt.
  $ echo 'let hello = "hello"' > lib_sub.ml

  $ dune runtest
  hello
  $ echo 'let _x = 1;; let () = print_endline "blah"' >> lib_sub.ml
  $ dune runtest
  blah
  hello
