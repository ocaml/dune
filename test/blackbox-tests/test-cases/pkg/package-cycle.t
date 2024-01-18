Package resolution creating a cycle

  $ . ./helpers.sh

  $ make_lockdir

  $ cat >dune.lock/a.pkg <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ cat >dune.lock/b.pkg <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ cat >dune.lock/c.pkg <<EOF
  > (version 0.0.1)
  > (depends a)
  > EOF

  $ build_pkg a
  Error: Dependency cycle between:
     - package a
  -> - package c
  -> - package b
  -> - package a
  [1]
