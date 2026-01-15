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
However, adding dune-project files and libraries with the same public name
causes a duplicate detection error when the library is used:
  $ cp dune-project ./vendor/duped/
  $ cp dune-project ./vendor/pkg/vendor/duped/
  $ cat >vendor/duped/dune <<EOF
  > (library (name duped) (public_name duped))
  > EOF
  $ cat >vendor/duped/duped.ml <<EOF
  > let x = 1
  > EOF
  $ cat >vendor/pkg/vendor/duped/dune <<EOF
  > (library (name duped) (public_name duped))
  > EOF
  $ cat >vendor/pkg/vendor/duped/duped.ml <<EOF
  > let x = 2
  > EOF
  $ cat >dune <<EOF
  > (vendored_dirs vendor)
  > (executable (name main) (libraries duped))
  > EOF
  $ cat >main.ml <<EOF
  > let () = print_int Duped.x
  > EOF
  $ dune build ./main.exe
  File "vendor/duped/dune", line 1, characters 0-42:
  1 | (library (name duped) (public_name duped))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library with name "duped" is already defined in
  vendor/pkg/vendor/duped/dune:1. Either change one of the names, or enable
  them conditionally using the 'enabled_if' field.
  [1]
