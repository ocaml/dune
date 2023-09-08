auto_open is a library field used to select which modules are going to be open
by default:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name foo))
  > EOF

  $ mkdir -p lib/sub bin

  $ cat >lib/sub/foo.ml <<EOF
  > let x = 1
  > EOF
  $ cat >lib/sub/bar.ml <<EOF
  > let y = 2
  > EOF

  $ make_lib() {
  > cat >lib/dune <<EOF
  > (library
  >  (name re_exporting_lib)
  >  (public_name foo)
  >  (libraries $1))
  > EOF
  > }

  $ make_auto_open() {
  > cat >lib/sub/dune <<EOF
  > (library
  >  (name myfoo)
  >  (public_name foo.sub)
  >  (wrapped false)
  >  (auto_open $1))
  > EOF
  > }

  $ cat >bin/dune <<EOF
  > (executable
  >  (name baz)
  >  (modules baz)
  >  (libraries foo))
  > EOF

  $ cat >bin/baz.ml <<EOF
  > print_int (x + y)
  > EOF

  $ make_lib "foo.sub"
  $ make_auto_open "Foo Bar"
  $ dune exec ./bin/baz.exe
  File "bin/baz.ml", line 1, characters 11-12:
  1 | print_int (x + y)
                 ^
  Error: Unbound value x
  [1]

  $ make_lib "(re_export foo.sub)"

  $ dune exec ./bin/baz.exe
  3

  $ make_auto_open "foo bar"
  $ dune exec ./bin/baz.exe
  3

  $ dune build foo.install
  $ cat >bin/dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ OCAMLPATH="$PWD/_build/install/default/lib" dune exec --root bin ./baz.exe
  Entering directory 'bin'
  Leaving directory 'bin'
  3
