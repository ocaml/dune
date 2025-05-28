This test is guarded by ocaml version <= 5.1, so it should not include foo
when implicit_transitive_deps is set to false, i.e. testing backward compatibility of
the new -H feature added.

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
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ dune build

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ getincludes
  -I .bar.objs/byte
  -I .bar.objs/byte
  -I .bar.objs/native
  -I .run.eobjs/byte
  -I .run.eobjs/byte
  -I .run.eobjs/native
