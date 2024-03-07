Test that we support the use case of using additional constraints to force the
system compiler to be used instead of installing the compiler.

  $ . ./helpers.sh

Create packages resembling the ocaml compiler packages
  $ mkrepo

  $ mkpkg ocaml-base-compiler <<EOF
  > conflicts: [ "ocaml-system" "ocaml-variants" ]
  > EOF

  $ mkpkg ocaml-variants <<EOF
  > conflicts: [ "ocaml-system" "ocaml-base-compiler" ]
  > EOF

  $ mkpkg ocaml-system <<EOF
  > conflicts: [ "ocaml-variants" "ocaml-base-compiler" ]
  > EOF

  $ mkpkg ocaml <<EOF
  > depends: [
  >  "ocaml-base-compiler" | "ocaml-variants" | "ocaml-system"
  > ]
  > EOF

A package that depends on ocaml:
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (depends ocaml))
  > EOF

Try solving without additional constraints:
  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for dune.lock:
  - ocaml.0.0.1
  - ocaml-base-compiler.0.0.1

Now make a workspace file adding the constarint on ocaml-system:
  $ cat >dune-workspace <<EOF
  > (lang dune 3.13)
  > (lock_dir
  >  (constraints ocaml-system)
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

Solve again. This time ocaml-system is chosen.
  $ dune pkg lock
  Solution for dune.lock:
  - ocaml.0.0.1
  - ocaml-system.0.0.1
