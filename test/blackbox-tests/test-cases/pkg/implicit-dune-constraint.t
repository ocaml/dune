By default, we introduce a constraint on in the build plan that will require
the dune version to match the version of dune being used to generate the
constraint. On another hand, we ensure `dune` can be used as a declared
dependency.

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

  $ test "2.0.0" 2>&1 | sed -E 's/3.[0-9]+/3.XX/g'
  File "default/.lock/_unknown_", line 1, characters 0-0:
  Error: Couldn't solve the package dependency formula.
  Selected candidates: foo.0.0.1 x.dev
  - dune -> (problem)
      User requested = 3.XX
      Rejected candidates:
        dune.3.XX.0: Incompatible with restriction: = 3.XX
  $ test "4.0.0"
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
