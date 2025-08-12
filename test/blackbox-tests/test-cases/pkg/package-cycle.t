Package resolution creating a cycle

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ make_lockpkg c <<EOF
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
