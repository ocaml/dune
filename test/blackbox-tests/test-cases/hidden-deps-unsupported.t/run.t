This test is guarded by OCaml version <= 5.1. On these compilers,
false-if-hidden-includes-supported preserves implicit transitive dependencies, so
foo remains visible with -I rather than -H.

  $ getincludes () {
  >   dune build --verbose ./run.exe 2>&1 | grep run.ml | grep -Eo '\-[IH] [a-z/.]+' | sort
  > }

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps true)
  > EOF

  $ getincludes
  -I .bar.objs/byte
  -I .bar.objs/native
  -I .foo.objs/byte
  -I .foo.objs/native
  -I .run.eobjs/byte
  -I .run.eobjs/byte
  -I .run.eobjs/native

The fallback setting requires Dune language 3.20:

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ dune build
  File "dune-project", line 2, characters 26-60:
  2 | (implicit_transitive_deps false-if-hidden-includes-supported)
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'false-if-hidden-includes-supported' is only available since version
  3.20 of the dune language. Please update your dune-project file to have (lang
  dune 3.20).
  [1]

Force a recompilation after enabling the fallback setting so getincludes prints
its compiler flags again:

  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (implicit_transitive_deps false-if-hidden-includes-supported)
  > EOF

  $ { cat run.ml; echo; } >run.ml.new && mv run.ml.new run.ml
  $ getincludes
  -I .bar.objs/byte
  -I .bar.objs/native
  -I .foo.objs/byte
  -I .foo.objs/native
  -I .run.eobjs/byte
  -I .run.eobjs/native
