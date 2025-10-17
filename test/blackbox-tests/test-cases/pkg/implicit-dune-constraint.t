By default, we introduce a constraint on in the build plan that will require
the dune version to match the version of dune being used to generate the
constraint. On another hand, we ensure `dune` can be used as a declared
dependency.

  $ . ./helpers.sh
  $ mkrepo

  $ test() {
  > mkpkg foo <<EOF
  > depends: [ "dune" {<= "$1"} ]
  > EOF
  > solve foo
  > }

  $ test "2.0.0" 2>&1 | sed -E 's/3.[0-9]+/3.XX/g'
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  Selected candidates: foo.0.0.1 x.dev
  - dune -> (problem)
      User requested = 3.XX
      foo 0.0.1 requires <= 2.0.0
      Rejected candidates:
        dune.3.XX: Incompatible with restriction: <= 2.0.0
  $ test "4.0.0"
  Solution for dune.lock:
  - foo.0.0.1

  $ test "4.0.0" 2>&1 | sed -E 's/3.[0-9]+/3.XX/g'
  Solution for dune.lock:
  - foo.0.0.1

Create a fake project and ensure `dune` can be used as a dependency:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends dune))
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  (no dependencies to lock)
