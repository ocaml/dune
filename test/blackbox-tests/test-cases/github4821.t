Ensure that a public, non-optional library can be disabled.
  $ touch foo.opam
  $ cat > dune-project<<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (public_name foo) (enabled_if false))
  > EOF
  $ dune build @install;
  > cat _build/install/default/lib/foo/dune-package | sed 's/dune [0-9].[0-9]/dune $version/';
  (lang dune $version)
  (name foo)
  (sections (lib .))
  (files (lib (META dune-package opam)))
