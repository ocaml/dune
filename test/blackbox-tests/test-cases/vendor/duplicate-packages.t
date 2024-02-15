This test verifies dune's behavior with respect to duplicate packages
First we verify that packages alone do not cause duplication errors:
  $ mkdir -p vendor/pkg/vendor/duped/
  $ mkdir -p vendor/duped/
  $ touch vendor/pkg/vendor/duped/duped.opam
  $ touch vendor/duped/duped.opam
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ cat >dune <<EOF
  > (vendored_dirs vendor)
  > EOF
  $ cat >vendor/pkg/dune <<EOF
  > (vendored_dirs vendor)
  > EOF
  $ dune build @all
However, adding dune-project files causes the duplicate detection to fire:
  $ cp dune-project ./vendor/duped/
  $ cp dune-project ./vendor/pkg/vendor/duped/
  $ dune build @all
  Error: The package "duped" is defined more than once:
  - vendor/pkg/vendor/duped/duped.opam:1
  - vendor/duped/duped.opam:1
  [1]
