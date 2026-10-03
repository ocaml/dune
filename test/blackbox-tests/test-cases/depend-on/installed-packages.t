# Test dependency on installed package

  $ mkdir a b prefix

  $ cat >a/dune-project <<EOF
  > (lang dune 2.9)
  > (package (name a))
  > EOF

  $ cat >a/dune <<EOF
  > (install (section share) (files CATME))
  > EOF

  $ cat >a/CATME <<EOF
  > Miaou
  > EOF

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/share/a/CATME

  $ cat >b/dune-project <<EOF
  > (lang dune 2.9)
  > (package (name b))
  > EOF

  $ cat >b/dune <<EOF
  > (rule (alias runtest) (deps (package a)) (action (run cat $PWD/prefix/share/a/CATME)))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'
  Miaou
  Leaving directory 'b'

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @runtest

  $ rm a/CATME
  $ cat >a/CATME <<EOF
  > Ouaf
  > EOF

  $ dune build --root a

  $ dune install --root a --prefix $PWD/prefix --display short
  Deleting $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Deleting $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Deleting $TESTCASE_ROOT/prefix/share/a/CATME
  Installing $TESTCASE_ROOT/prefix/share/a/CATME

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'
  Ouaf
  Leaving directory 'b'

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root b @runtest

Installed packages also work with Dune language versions older than 2.9.

  $ cat >b/dune-project <<EOF
  > (lang dune 2.8)
  > (package (name b))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH \
  >   dune build --root b --build-dir _build_2_8 @runtest
  Entering directory 'b'
  Ouaf
  Leaving directory 'b'
