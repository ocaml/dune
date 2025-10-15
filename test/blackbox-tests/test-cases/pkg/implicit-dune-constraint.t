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
  The following packages couldn't be found: dune
  $ test "4.0.0"
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: dune
  [1]

  $ test "4.0.0" 2>&1 | sed -E 's/3.[0-9]+/3.XX/g'
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: dune

Create a fake project and ensure `dune` can be used as a dependency:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends dune))
  > EOF
  $ dune pkg lock
  Error: Unable to solve dependencies for the following lock directories:
  Lock directory dune.lock:
  Couldn't solve the package dependency formula.
  The following packages couldn't be found: dune
  [1]
