This test is guarded by ocaml version >= 5.2, so it should include foo with -H when
implicit_transitive_deps is set to false.

  $ getincludes () {
  >   dune build --verbose ./run.exe 2>&1 | grep run.ml | grep -Eo '\-[IH] [a-z/.]+' | sort
  > }

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps true)
  > EOF

  $ getincludes
  -I .bar.objs/byte
  -I .bar.objs/byte
  -I .bar.objs/native
  -I .foo.objs/byte
  -I .foo.objs/byte
  -I .foo.objs/native
  -I .run.eobjs/byte
  -I .run.eobjs/byte
  -I .run.eobjs/native

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false)
  > EOF

  $ getincludes
  -H .foo.objs/byte
  -H .foo.objs/byte
  -H .foo.objs/native
  -I .bar.objs/byte
  -I .bar.objs/byte
  -I .bar.objs/native
  -I .run.eobjs/byte
  -I .run.eobjs/byte
  -I .run.eobjs/native

Test transitive deps can not be directly accessed, both for compiler versions supporting -H or not:

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false)
  > EOF

  $ dune build ./runf.exe 2>&1 | grep -v ocamlc
  File "runf.ml", line 1, characters 16-21:
  1 | let a = Bar.y + Foo.v
                      ^^^^^
  Error: Unbound module Foo

Test if #274 is fixed:

  $ dune build --root=./tyxml
  Entering directory 'tyxml'
  Leaving directory 'tyxml'
