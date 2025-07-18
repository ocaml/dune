We test that installed Coq theories, when updated, prompt Dune to rebuild

  $ mkdir -p lib/coq
  $ export ROCQLIB=$PWD/lib/coq
  $ echo $ROCQLIB
  $TESTCASE_ROOT/lib/coq

  $ mkdir -p lib/coq/theories/Init/
  $ cat > lib/coq/theories/Init/Prelude.v << EOF
  > Inductive PreludeLoaded := Yes.
  > EOF

We need to manually compile the prelude.

  $ coqc -boot -noinit -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler no -R lib/coq/theories/ Corelib lib/coq/theories/Init/Prelude.v

We also setup some plugins

  $ mkdir -p lib/rocq-runtime/plugins

We setup an installed theory. Note that lib/coq/user-contrib doesn't exist yet,
so this also tests that it won't be a problem.

  $ cat > B/b.v << EOF
  > Inductive hello := I | am | an | install | loc.
  > EOF

  $ dune build --root B @install
  Entering directory 'B'
  Leaving directory 'B'
  $ dune install --root B --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/rocq-package


Next we go into our Dune project and build it.
  $ dune build --root A
  Entering directory 'A'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/B
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.
  Leaving directory 'A'

Next we update B and install it again.

  $ cat > B/b.v << EOF
  > Inductive hello := I | am | an | install | loc | but | updated.
  > EOF

  $ dune build --root B @install
  Entering directory 'B'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/B
  Leaving directory 'B'
  $ dune install --root B --prefix=$PWD --display=short
  Deleting $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/META
  Deleting $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/rocq-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/rocq-package

Now we should see that A is rebuilt

  $ dune build --root A
  Entering directory 'A'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/B
  Inductive hello : Set :=
      I : hello
    | am : hello
    | an : hello
    | install : hello
    | loc : hello
    | but : hello
    | updated : hello.
  Leaving directory 'A'

Next we add a new file to B that should cause a call to coqdep, but no rebuild.

  $ cat > B/c.v << EOF
  > Inductive bye := I | am | a | new | install | loc.
  > EOF

  $ dune build --root B @install
  Entering directory 'B'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/B
  Leaving directory 'B'
  $ dune install --root B --prefix=$PWD --display=short
  Deleting $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/META
  Deleting $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/c.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/c.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/c.vo
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/rocq-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/rocq-package

Now we should see that A is not rebuilt, however coqdep is called, this seems to fail

  $ dune build --root A --display=short
  Entering directory 'A'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/B
  Leaving directory 'A'
