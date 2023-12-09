This test demonstrates a local package that's in the same conflict-class of a
dependency.

  $ . ./helpers.sh

  $ mkrepo

  $ mkpkg conflict-class

  $ mkpkg bar <<EOF
  > conflict-class: "ccc"
  > EOF

Local conflict class defined in a local package:

  $ cat >foo.opam <<EOF
  > depends: [ "bar" ]
  > conflict-class: "ccc"
  > EOF

  $ cat >x.opam <<EOF
  > depends: "foo"
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)

Now the conflict class comes from the opam repository

  $ mkpkg foo <<EOF
  > depends: [ "bar" ]
  > conflict-class: "ccc"
  > EOF

  $ rm foo.opam

  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
