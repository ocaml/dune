This test demonstrates a local package that's in the same conflict-class of a
dependency.

  $ . ./helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkpkg conflict-class

  $ mkpkg bar <<EOF
  > conflict-class: "ccc"
  > EOF

Local conflict class defined in a local package:

  $ cat >foo.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "bar" ]
  > conflict-class: "ccc"
  > EOF

  $ cat >x.opam <<EOF
  > opam-version: "2.0"
  > depends: "foo"
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: foo.dev x.dev foo&x
  - bar -> (problem)
      Rejected candidates:
        bar.0.0.1: In same conflict class (ccc) as foo
  [1]

Now the conflict class comes from the opam repository

  $ mkpkg foo <<EOF
  > depends: [ "bar" ]
  > conflict-class: "ccc"
  > EOF

  $ rm foo.opam

  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: foo.0.0.1 x.dev
  - bar -> (problem)
      Rejected candidates:
        bar.0.0.1: In same conflict class (ccc) as foo
  [1]
