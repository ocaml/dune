Requesting to build a single package should not build unrelated things:

  $ . ./helpers.sh

  $ make_lockdir

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

  $ pkg() {
  > make_lockpkg $1 <<EOF
  > (build (run echo building $1))
  > (version dev)
  > EOF
  > }

These two packages are independent:

  $ pkg foo
  $ pkg bar

We should only see the result of building "foo"

  $ build_pkg foo
  building foo

We should only see the result of building "bar"

  $ build_pkg bar
  building bar
