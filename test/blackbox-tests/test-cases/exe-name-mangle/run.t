Binary composed of a single module with the same name as the dependency
  $ dune build --root single-module 2>&1 | grep -v ocamlopt
  Entering directory 'single-module'
  File "exe.ml", line 1:
  Error: The files foo/.foo.objs/byte/foo.cmi and .exe.eobjs/byte/exe.cmi
         make inconsistent assumptions over interface Exe
Binary composed of multiple modules where one collides with a dependency
  $ dune build --root multi-module 2>&1 | grep -v ocamlopt | grep -v ocamlc
  Entering directory 'multi-module'
  File "foo.ml", line 1:
  Error: The files foo/.foo.objs/byte/bar.cmi and .baz.eobjs/byte/foo.cmi
         make inconsistent assumptions over interface Foo
  File "baz.ml", line 1:
  Error: The files .baz.eobjs/byte/foo.cmi and .baz.eobjs/byte/foo.cmi
         make inconsistent assumptions over interface Foo
