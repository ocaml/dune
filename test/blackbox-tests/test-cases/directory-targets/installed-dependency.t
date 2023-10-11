Allow directories to be installable

  $ mkdir a b prefix
  $ cat >a/dune-project <<EOF
  > (lang dune 3.5)
  > (package (name foo))
  > (using directory-targets 0.1)
  > EOF
  $ cat >a/dune <<EOF
  > (install
  >  (dirs rules/bar)
  >  (section share))
  > EOF
  $ mkdir a/rules
  $ cat >a/rules/dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (deps (sandbox always))
  >  (action (bash "mkdir -p %{target}/baz && touch %{target}/{x,y,z} && touch %{target}/baz/{a,b}")))
  > EOF
  $ dune build --root=a foo.install
  Entering directory 'a'
  Leaving directory 'a'

  $ cat a/_build/install/default/lib/foo/dune-package
  (lang dune 3.11)
  (name foo)
  (sections (lib .) (share ../../share/foo))
  (files
   (lib (META dune-package))
   (share (bar/baz/a bar/baz/b bar/x bar/y bar/z)))
  $ dune install --root a --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/share/foo/bar/baz/a
  Installing $TESTCASE_ROOT/prefix/share/foo/bar/baz/b
  Installing $TESTCASE_ROOT/prefix/share/foo/bar/x
  Installing $TESTCASE_ROOT/prefix/share/foo/bar/y
  Installing $TESTCASE_ROOT/prefix/share/foo/bar/z

  $ cat > b/dune-project <<EOF
  > (lang dune 3.5)
  > EOF
  $ cat > b/dune <<EOF
  > (alias (name foo) (deps (package foo)))
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root=b @foo --display=short
  Entering directory 'b'
  Leaving directory 'b'
