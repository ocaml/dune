dune files
==========

%{dep:string}
-------------

In expands to a file name, and registers this as a dependency.

  $ dune build --root dune @test-dep
  Entering directory 'dune'
  File "dune", line 13, characters 17-47:
  Error: Variable %{path:file-that-does-not-exist} has been renamed to %{dep:file-that-does-not-exist} since 1.0
  [1]

%{path-no-dep:string}
---------------------

This form does not exist, but displays an hint:

  $ dune build --root dune-invalid @test-path-no-dep
  Entering directory 'dune-invalid'
  File "dune", line 7, characters 17-54:
  Error: The ${path-no-dep:...} syntax has been removed from dune.
  [1]

jbuild files
============

${path:string}
--------------

This registers the dependency:

  $ dune build --root jbuild @test-path
  Entering directory 'jbuild'
  dynamic-contents

${path-no-dep:string}
---------------------

This does not:

  $ dune build --root jbuild @test-path-no-dep
  Entering directory 'jbuild'
  ../../file-that-does-not-exist
  ../..

${dep:string}
--------------

This form does not exist, but displays an hint:

  $ dune build --root jbuild-invalid @test-dep
  Entering directory 'jbuild-invalid'
  File "jbuild", line 5, characters 16-37:
  Error: Variable ${dep:generated-file} is available in since version 1.0. Current version is 0.0
  [1]
