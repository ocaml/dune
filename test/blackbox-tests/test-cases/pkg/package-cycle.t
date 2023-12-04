Package resolution creating a cycle

  $ . ./helpers.sh

  $ make_lockdir

  $ cat >dune.lock/a.pkg <<EOF
  > (deps b)
  > EOF
  $ cat >dune.lock/b.pkg <<EOF
  > (deps c)
  > EOF
  $ cat >dune.lock/c.pkg <<EOF
  > (deps a)
  > EOF

  $ build_pkg a
  Error: Dependency cycle between:
     - package a
  -> - package c
  -> - package b
  -> - package a
  [1]
