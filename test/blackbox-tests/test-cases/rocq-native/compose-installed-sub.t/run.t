We test composing a project with an installed Coq theory. The installed theory
does *not* have to be a dune project. But for the purpose of this test, we use
the installation of a Dune project.

We configure ROCQLIB to be lib/coq. Coq will search for user-contrib from here.
We also need to set up a fake Coq install.

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

Setting up a subdirectory theory:

  $ cat > user/dune << EOF
  > (rocq.theory
  >  (name user)
  >  (theories global.field global.algebra))
  > EOF

We setup an installed theory. Note that lib/coq/user-contrib doesn't exist yet,
so this also tests that it won't be a problem.

  $ dune build --root global @install
  Entering directory 'global'
  Leaving directory 'global'
  $ dune install --root global --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/global/META
  Installing $TESTCASE_ROOT/lib/global/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/rocq-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/rocq-package


Next we go into our Dune project and build it.
  $ dune build --root user
  Entering directory 'user'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/global/field
  Inductive hello_alg : Set :=
      I : hello_alg
    | am : hello_alg
    | an : hello_alg
    | install : hello_alg
    | loc : hello_alg
    | at_alg : hello_alg.
  Inductive hello_field : Set :=
      I : hello_field
    | am : hello_field
    | an : hello_field
    | install : hello_field
    | loc : hello_field
    | at_field : hello_field.
  Leaving directory 'user'

Next we uninstall B from the standard location, user-contrib, and install it
somewhere else.
  $ dune uninstall --root global --prefix=$PWD --display=short
  Deleting $TESTCASE_ROOT/lib/global/META
  Deleting $TESTCASE_ROOT/lib/global/dune-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmi
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmxs
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.glob
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.vo
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/rocq-package
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmi
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmxs
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.glob
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.v
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.vo
  Deleting $TESTCASE_ROOT/lib/coq/user-contrib/global/field/rocq-package
  Deleting empty directory $TESTCASE_ROOT/lib/global
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/global/field
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native
  Deleting empty directory $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra

  $ dune install --root global --prefix=$PWD/another-place --display=short
  Installing $TESTCASE_ROOT/another-place/lib/global/META
  Installing $TESTCASE_ROOT/another-place/lib/global/dune-package
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmi
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmxs
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/b_alg.glob
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/b_alg.v
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/b_alg.vo
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra/rocq-package
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmi
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmxs
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/b_field.glob
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/b_field.v
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/b_field.vo
  Installing $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field/rocq-package
  $ rmdir lib/coq/user-contrib/global

As expected, Dune can no longer build A:

  $ dune build --root user
  Entering directory 'user'
  File "dune", line 3, characters 11-23:
  3 |  (theories global.field global.algebra))
                 ^^^^^^^^^^^^
  Theory "global.field" has not been found.
  -> required by theory user in dune:2
  -> required by _build/default/.user.theory.d
  -> required by alias all
  -> required by alias default
  Leaving directory 'user'
  [1]

We therefore set a variable called ROCQPATH which allows for library install
locations alternative to user-contrib.

  $ export ROCQPATH=$PWD/another-place/lib/coq/user-contrib

Now Dune should be able to build `user` again, since we scan both user-contrib and
all the directories found in ROCQPATH.

  $ dune build --root user
  Entering directory 'user'
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field
  Inductive hello_alg : Set :=
      I : hello_alg
    | am : hello_alg
    | an : hello_alg
    | install : hello_alg
    | loc : hello_alg
    | at_alg : hello_alg.
  Inductive hello_field : Set :=
      I : hello_field
    | am : hello_field
    | an : hello_field
    | install : hello_field
    | loc : hello_field
    | at_field : hello_field.
  Leaving directory 'user'

We test if having global in the workspace and in user-contrib will cause Dune
any problems. It shouldn't do, as the workspace should take precedence.

  $ dune build user
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field
  Inductive hello_alg : Set :=
      I : hello_alg
    | am : hello_alg
    | an : hello_alg
    | install : hello_alg
    | loc : hello_alg
    | at_alg : hello_alg.
  Inductive hello_field : Set :=
      I : hello_field
    | am : hello_field
    | an : hello_field
    | install : hello_field
    | loc : hello_field
    | at_field : hello_field.

We test updating the dune file for user to use the super-theory works:

  $ cat > user/dune << EOF
  > (rocq.theory
  >  (name user)
  >  (theories global))
  > EOF
  $ dune build --root user
  Entering directory 'user'
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field
  Inductive hello_alg : Set :=
      I : hello_alg
    | am : hello_alg
    | an : hello_alg
    | install : hello_alg
    | loc : hello_alg
    | at_alg : hello_alg.
  Inductive hello_field : Set :=
      I : hello_field
    | am : hello_field
    | an : hello_field
    | install : hello_field
    | loc : hello_field
    | at_field : hello_field.
  Leaving directory 'user'

We test whether installing `global` again in user-contrib will cause Dune to reject the
build. Currently this is not the case and the first theory is preferred inline
with the loadpath semantics of Coq.

  $ dune install --root global --prefix=$PWD --display=short
  Installing $TESTCASE_ROOT/lib/global/META
  Installing $TESTCASE_ROOT/lib/global/dune-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/.coq-native/Nglobal_algebra_b_alg.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/b_alg.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra/rocq-package
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmi
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/.coq-native/Nglobal_field_b_field.cmxs
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.glob
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.v
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/b_field.vo
  Installing $TESTCASE_ROOT/lib/coq/user-contrib/global/field/rocq-package

  $ dune build --root user
  Entering directory 'user'
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/another-place/lib/coq/user-contrib/global/field
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/global/algebra
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/global/field
  Leaving directory 'user'
