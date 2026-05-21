By default, we introduce a constraint on in the build plan that will require
the dune version to match the version of dune being used to generate the
constraint. On another hand, we ensure `dune` can be used as a declared
dependency.

  $ mkrepo

  $ test() {
  > mkpkg foo <<EOF
  > depends: [ "dune" {<= "$1"} ]
  > EOF
  > solve foo
  > }

Upper bounds on dune are stripped, so this now locks against the running
dune.
  $ test "2.0.0"
  Solution for dune.lock:
  - foo.0.0.1
  $ test "4.0.0"
  Solution for dune.lock:
  - foo.0.0.1

  $ test "4.0.0" 2>&1 | dune_cmd subst '3.[0-9]+' '3.XX'
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
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  (no dependencies to lock)
