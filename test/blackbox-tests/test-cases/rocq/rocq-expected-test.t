Testing the expected test support.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (rocq.theory
  >  (name a))
  > EOF

  $ cat > foo.v <<EOF
  > Locate nat.
  > EOF

  $ mkdir sub
  $ cat > sub/bar.v <<EOF
  > Locate bool.
  > EOF

  $ dune build 2>&1 | sort
  Inductive Corelib.Init.Datatypes.bool
  Inductive Corelib.Init.Datatypes.nat

  $ dune clean
  $ touch foo.expected
  $ dune build
  Inductive Corelib.Init.Datatypes.bool
  $ dune runtest
  File "foo.expected", line 1, characters 0-0:
  --- foo.expected
  +++ foo.output
  @@ -0,0 +1 @@
  +Inductive Corelib.Init.Datatypes.nat
  [1]

  $ dune promote
  Promoting _build/default/foo.output to foo.expected.
  $ cat foo.expected
  Inductive Corelib.Init.Datatypes.nat
  $ dune runtest

  $ touch sub/bar.expected
  $ dune build
  $ dune runtest
  File "sub/bar.expected", line 1, characters 0-0:
  --- sub/bar.expected
  +++ sub/bar.output
  @@ -0,0 +1 @@
  +Inductive Corelib.Init.Datatypes.bool
  [1]

  $ dune promote
  Promoting _build/default/sub/bar.output to sub/bar.expected.
  $ cat sub/bar.expected
  Inductive Corelib.Init.Datatypes.bool
  $ dune runtest
