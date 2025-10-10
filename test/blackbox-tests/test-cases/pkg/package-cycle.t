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
  Error: Dependency cycle between packages:
     a.0.0.1
  -> b.0.0.1
  -> c.0.0.1
  -> a.0.0.1
  [1]
