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

In the following two tests we use "false-if-hidden-includes-supported" for
testing purposes, but since this test is guarded by OCaml version >= 5.2, this
should be equivalent to "false".

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
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
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ dune build ./runf.exe 2>&1 | grep -v ocamlc
  File "runf.ml", line 1, characters 16-19:
  1 | let _ = Bar.y + Foo.v
                      ^^^
  Error: Unbound module Foo
