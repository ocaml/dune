We test composing a project with an installed Coq theory. The installed theory
does *not* have to be a dune project. But for the purpose of this test, we use
the installation of a Dune project.

We configure COQLIB to be lib/coq. Coq will search for user-contrib from here.
We also need to set up a fake Coq install.

  $ mkdir -p lib/coq
  $ export COQLIB=$PWD/lib/coq
  $ echo $COQLIB
  $TESTCASE_ROOT/lib/coq

  $ mkdir -p lib/coq/theories/Init/
  $ cat > lib/coq/theories/Init/Prelude.v << EOF
  > Inductive PreludeLoaded := Yes.
  > EOF

We need to manually compile the prelude.

  $ coqc -boot -noinit -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler no -R lib/coq/theories/ Coq lib/coq/theories/Init/Prelude.v

We also setup some plugins

  $ mkdir -p lib/coq-core/plugins

We setup an installed theory. Note that lib/coq/user-contrib doesn't exist yet,
so this also tests that it won't be a problem.

  $ dune build --root B @install
  Entering directory 'B'
  Leaving directory 'B'
  $ dune install --root B --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo


Next we go into our Dune project and build it.
  $ dune build --root A
  Entering directory 'A'
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.
  Leaving directory 'A'

Next we uninstall B from the standard location, user-contrib, and install it
somewhere else.
  $ dune uninstall --root B --prefix=$PWD --display=short
  Deleting $TESTCASE_ROOT/lib/B/META
  Deleting $TESTCASE_ROOT/lib/B/dune-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/B
  Deleting empty directory $TESTCASE_ROOT/lib/B

  $ dune install --root B --prefix=$PWD/another-place --display=short
  Installing $TESTCASE_ROOT/another-place/lib/B/META
  Installing $TESTCASE_ROOT/another-place/lib/B/dune-package
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/B/b.vo

As expected, Dune can no longer build A:

  $ dune build --root A
  Entering directory 'A'
  File "dune", line 3, characters 15-16:
  3 |  (theories Coq B))
                     ^
  Theory "B" has not been found.
  -> required by theory A in dune:2
  -> required by _build/default/.A.theory.d
  -> required by alias all
  -> required by alias default
  Leaving directory 'A'
  [1]

We therefore set a variable called COQPATH which allows for library install
locations alternative to user-contrib.

  $ export COQPATH=$PWD/another-place/lib/coq/user-contrib

Now Dune should be able to build A again, since we scan both user-contrib and
all the directories found in COQPATH.

  $ dune build --root A
  Entering directory 'A'
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.
  Leaving directory 'A'

We test if having B in the workspace and in user-contrib will cause Dune
any problems. It shouldn't do, as the workspace should take precedence.

  $ dune build A
  Inductive hello : Set :=
      I : hello | am : hello | an : hello | install : hello | loc : hello.

We test whether installing B again in user-contrib will cause Dune to reject the
build. Currently this is not the case and the first theory is preferred inline
with the loadpath semantics of Coq.

  $ dune install --root B --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/B/META
  Installing $TESTCASE_ROOT/lib/B/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/.coq-native/NB_b.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/B/b.vo

  $ dune build --root A
  Entering directory 'A'
  Leaving directory 'A'
