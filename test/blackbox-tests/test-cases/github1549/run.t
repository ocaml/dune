Reproduction case for #1549: too many parentheses in installed .dune files

  $ dune build --root backend
  Entering directory 'backend'

  $ cat backend/_build/install/default/lib/dune_inline_tests/dune_inline_tests.dune
  (dune
   2
   ((inline_tests.backend
     1.0
     ((runner_libraries ())
      (flags :standard)
      (generate_runner
       (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))))

  $ env OCAMLPATH=backend/_build/install/default/lib dune runtest --root example
  Entering directory 'example'
  File "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/github1549/backend/_build/install/default/lib/dune_inline_tests/dune_inline_tests.dune", line 5, characters 3-141:
  5 |    ((runner_libraries ())
  6 |     (flags :standard)
  7 |     (generate_runner
  8 |      (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))))
  Error: Atom expected
  Hint: dune files require less parentheses than jbuild files.
  If you just converted this file from a jbuild file, try removing these parentheses.
  [1]
