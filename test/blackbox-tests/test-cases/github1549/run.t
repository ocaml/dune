Reproduction case for #1549: too many parentheses in installed .dune files

  $ dune build --root backend
  Entering directory 'backend'

  $ cat backend/_build/install/default/lib/dune_inline_tests/dune_inline_tests.dune
  (dune
   2
   ((inline_tests.backend
     1.0
     ((flags :standard)
      (generate_runner
       (run sed "s/(\\*TEST:\\(.*\\)\\*)/let () = \\1;;/" %{impl-files}))))))

  $ env OCAMLPATH=backend/_build/install/default/lib dune runtest --root example
  Entering directory 'example'
