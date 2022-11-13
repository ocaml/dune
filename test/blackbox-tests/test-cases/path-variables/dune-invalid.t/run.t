%{path-no-dep:string}
---------------------

This form does not exist, but displays an hint:

  $ dune build @test-path-no-dep
  File "dune", line 7, characters 15-54:
  7 |         (echo "%{path-no-dep:file-that-does-not-exist}\n")
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{path-no-dep:..} was deleted in version 1.0 of the dune language.
  File "dune", line 8, characters 15-31:
  8 |         (echo "%{path-no-dep:.}\n")))))
                     ^^^^^^^^^^^^^^^^
  Error: %{path-no-dep:..} was deleted in version 1.0 of the dune language.
  [1]
