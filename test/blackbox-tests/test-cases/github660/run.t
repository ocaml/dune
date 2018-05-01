When there are explicit interfaces, modules must be rebuilt.

  $ dune runtest --root explicit-interfaces --display quiet -j1 2>&1 | grep -v Entering
          main alias runtest
  hello
  $ echo 'let x = 1' >> explicit-interfaces/lib_sub.ml
  $ dune runtest --root explicit-interfaces --display quiet -j1 2>&1 | grep -v Entering | grep -v ocamlopt
          main alias runtest
  hello

When there are no interfaces, the situation is the same, but it is not possible
to rely on these.

  $ dune runtest --root no-interfaces --display quiet -j1 2>&1 | grep -v Entering
          main alias runtest
  hello
  $ echo 'let x = 1' >> no-interfaces/lib_sub.ml
  $ dune runtest --root no-interfaces --display quiet -j1 2>&1 | grep -v Entering | grep -v ocamlopt
          main alias runtest
  hello
