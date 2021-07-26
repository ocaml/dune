  $ touch foo.opam
  $ cat > dune-project<<EOF
  > (lang dune 2.0)
  > EOF
  $ cat > dune <<EOF
  > (library (name foo) (public_name foo) (enabled_if false))
  > EOF
  $ dune build @install;
  > cat _build/default/foo.install;
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/opam"
  ]
