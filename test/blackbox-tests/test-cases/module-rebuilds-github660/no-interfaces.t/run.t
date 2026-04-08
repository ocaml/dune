When there are no interfaces, the situation is the same, but it
is not possible to rely on these.

  $ echo 'let hello = "hello"' > lib_sub.ml

  $ dune runtest
  hello
  $ echo 'let _x = 1' >> lib_sub.ml
  $ dune runtest
  hello
