Package resolution creating a cycle

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
  Error: Dependency cycle between packages:
     b
  -> c
  -> a
  -> b
  -> required by _build/default/.pkg/a/_install
  [1]
