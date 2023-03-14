This test demonstrates a dependency being pulled via an mli only module:

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ lib=foo

  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (wrapped false)
  >  (modules_without_implementation uast))
  > EOF

  $ cat >identifier.ml <<EOF
  > let create () = ()
  > EOF
  $ cat >identifier.mli <<EOF
  > val create : unit -> unit
  > EOF

  $ cat >uast.mli <<EOF
  > module Identifier = Identifier
  > EOF

  $ cat >upretty_printer.ml <<EOF
  > let f () = Uast.Identifier.create ()
  > EOF

  $ mkdir bin
  $ cat >bin/dune <<EOF
  > (executable
  >  (name use)
  >  (libraries $lib))
  > EOF

  $ cat >bin/use.ml <<EOF
  > let () = Upretty_printer.f ()
  > EOF

Build in development mode:

  $ dune exec bin/use.exe

And now in release

  $ dune exec bin/use.exe --release 2>&1 | grep -v "cd _build"
  [1]
