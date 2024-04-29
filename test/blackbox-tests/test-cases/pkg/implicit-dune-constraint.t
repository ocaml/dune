By default, we introduce a constraint on in the build plan that will require
the dune version to match the version of dune being used to generate the
constraint.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg dune 3.11.0 <<EOF
  > EOF

  $ test() {
  > mkpkg foo <<EOF
  > depends: [ "dune" {<= "$1"} ]
  > EOF
  > solve foo
  > }

  $ test "2.0.0"
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Can't find all required versions.
  Selected: foo.0.0.1 x.dev
  - dune -> (problem)
      User requested = 3.16
      Rejected candidates:
        dune.3.11.0: Incompatible with restriction: = 3.16
  [1]
  $ test "4.0.0"
  Solution for dune.lock:
  - dune.3.11.0
  - foo.0.0.1
