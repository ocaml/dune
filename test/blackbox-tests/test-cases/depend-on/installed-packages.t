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
  Entering directory 'a'

  $ dune install --root a --prefix $(pwd)/prefix
  Entering directory 'a'
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/share/a/CATME

  $ cat >b/dune-project <<EOF
  > (lang dune 2.9)
  > (package (name b))
  > EOF

  $ cat >b/dune <<EOF
  > (rule (alias runtest) (deps (package a)) (action (run cat $(pwd)/prefix/share/a/CATME)))
  > EOF

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'
  Miaou

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'

  $ rm a/CATME
  $ cat >a/CATME <<EOF
  > Ouaf
  > EOF

  $ dune build --root a
  Entering directory 'a'

  $ dune install --root a --prefix $(pwd)/prefix
  Entering directory 'a'
  Deleting $TESTCASE_ROOT/prefix/lib/a/META
  Installing $TESTCASE_ROOT/prefix/lib/a/META
  Deleting $TESTCASE_ROOT/prefix/lib/a/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/a/dune-package
  Deleting $TESTCASE_ROOT/prefix/share/a/CATME
  Installing $TESTCASE_ROOT/prefix/share/a/CATME

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'
  Ouaf

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'

  $ cat >b/dune-project <<EOF
  > (lang dune 2.8)
  > (package (name b))
  > EOF

  $ OCAMLPATH=$(pwd)/prefix/lib/:$OCAMLPATH dune build --root b @runtest
  Entering directory 'b'
  File "dune", line 1, characters 37-38:
  1 | (rule (alias runtest) (deps (package a)) (action (run cat $TESTCASE_ROOT/prefix/share/a/CATME)))
                                           ^
  Error: Dependency on an installed package requires at least (lang dune 2.9)
  [1]
