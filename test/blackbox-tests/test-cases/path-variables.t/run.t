dune files
==========

%{dep:string}
-------------

In expands to a file name, and registers this as a dependency.

  $ dune build --root dune @test-dep
  Entering directory 'dune'
  dynamic-contents

%{path-no-dep:string}
---------------------

This form does not exist, but displays an hint:

  $ dune build --root dune-invalid @test-path-no-dep
  Entering directory 'dune-invalid'
  File "dune", line 7, characters 15-54:
  7 |         (echo "%{path-no-dep:file-that-does-not-exist}\n")
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{path-no-dep:..} was deleted in version 1.0 of the dune language.
  File "dune", line 8, characters 15-31:
  8 |         (echo "%{path-no-dep:.}\n")))))
                     ^^^^^^^^^^^^^^^^
  Error: %{path-no-dep:..} was deleted in version 1.0 of the dune language.
  [1]
