We test that installed Coq theories, when updated, prompt Dune to rebuild

  $ mkdir -p lib/coq
  $ export COQLIB=$PWD/lib/coq
  $ echo $COQLIB
  $TESTCASE_ROOT/lib/coq

  $ mkdir -p lib/coq/theories/Init/
  $ echo > lib/coq/theories/Init/Prelude.v << EOF
  > Inductive PreludeLoaded := Yes.
  > EOF

We need to manually compile the prelude.

  $ coqc -boot -noinit -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler no -R lib/coq/theories/ Coq lib/coq/theories/Init/Prelude.v

We also setup some plugins

  $ mkdir -p lib/coq-core/plugins

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
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo


Next we go into our Dune project and build it.
  $ dune build --root A
  Entering directory 'A'
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.
  Leaving directory 'A'

Next we update B and install it again.

  $ cat > B/b.v << EOF
  > Inductive hello := I | am | an | install | loc | but | updated.
  > EOF

  $ dune build --root B @install
  Entering directory 'B'
  Leaving directory 'B'
  $ dune install --root B --prefix=$PWD --display=short
  Deleting $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/META
  Deleting $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo

Now we should see that A is rebuilt

  $ dune build --root A
  Entering directory 'A'
  Inductive hello : Set :=
      I : hello
    | am : hello
    | an : hello
    | install : hello
    | loc : hello
    | but : hello
    | updated : hello.
  Leaving directory 'A'
