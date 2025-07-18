Tests for meta-data for installed theories built with Dune. c.f.

- Rocq cannot find the cmxs file for transitive ocaml dependencies (https://github.com/ocaml/dune/issues/11012)
- Dune-Rocq: support transitive dependencies even for installed theories (https://github.com/ocaml/dune/issues/11483)

In particular, we will build a set of theories that depend on each
other, then install them. Then our user library will depend on the
outer one, and dune should realize the transitive dependencie (both
for .vo and ML plugins)

  $ dune build --root to_install @all
  Entering directory 'to_install'
  Hello
  Hello
  Hello
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ ROCQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/T_a
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/T_b
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/T_c
  TEST: $TESTCASE_ROOT/lib/coq/user-contrib/T_d
  File "$TESTCASE_ROOT/lib/coq/user-contrib/T_a/rocq-package", line 3, characters 9-22:
  3 | (plugins global.plugin)
               ^^^^^^^^^^^^^
  Error: Library "global.plugin" not found.
  -> required by theory T_a in
     $TESTCASE_ROOT/lib/coq/user-contrib/T_a/rocq-package:2
  -> required by _build/default/.User.theory.d
  -> required by alias all
  Leaving directory 'user'
  [1]
