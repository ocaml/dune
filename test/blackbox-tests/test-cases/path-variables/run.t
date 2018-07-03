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

It expands to a file name, but does not register it as a dependency.

  $ dune build --root dune @test-path-no-dep
  Entering directory 'dune'
  ../../file-that-does-not-exist
  ../..

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
  Error: ${dep:generated-file} is not supported in jbuild files.
  Did you mean: ${path:generated-file}
  [1]
