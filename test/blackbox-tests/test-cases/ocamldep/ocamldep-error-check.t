Dune uses ocamldep to prevent a module from depending on itself.

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (wrapped_executables false)
  > EOF

  $ mkdir lib
  $ cat >lib/dune <<EOF
  > (library
  >  (name foo))
  > EOF
  $ cat >lib/bar.ml <<EOF
  > Foo.bar
  > EOF
  $ dune build @all
  Error: Module Bar in directory _build/default/lib depends on Foo.
  This doesn't make sense to me.
  
  Foo is the main module of the library and is the only module exposed outside
  of the library. Consequently, it should be the one depending on all the other
  modules in the library.
  -> required by _build/default/lib/.foo.objs/foo__Bar.impl.all-deps
  -> required by _build/default/lib/.foo.objs/byte/foo__Bar.cmo
  -> required by _build/default/lib/foo.cma
  -> required by alias lib/all
  [1]

  $ rm lib/bar.ml

This check doesn't apply to single module libraries:

  $ cat >lib/foo.ml <<EOF
  > let x = Foo.x
  > EOF
  $ dune build @all
  File "lib/foo.ml", line 1, characters 8-13:
  1 | let x = Foo.x
              ^^^^^
  Error: Unbound module Foo
  [1]
  $ rm lib/foo.ml

However, we'll demonstrate that this check isn't applicable to executables:

  $ cat >lib/bar.ml <<EOF
  > let run () = print_endline "Hello World"
  > EOF

  $ mkdir exe
  $ cat >exe/dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries foo))
  > EOF
  $ cat >exe/foo.ml <<EOF
  > Foo.Bar.run ();;
  > EOF

Although we get slightly different behavior if wrapping is on or off:

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (wrapped_executables false)
  > EOF
  $ dune exec ./exe/foo.exe
  File "exe/foo.ml", line 1, characters 0-11:
  1 | Foo.Bar.run ();;
      ^^^^^^^^^^^
  Error: Unbound module Foo
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (wrapped_executables true)
  > EOF
  $ dune exec ./exe/foo.exe
  Hello World
