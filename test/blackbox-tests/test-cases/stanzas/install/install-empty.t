Empty install entry lists are accepted. The install manifest contains only
the package metadata.

  $ make_dune_project 3.11
  $ touch foo.opam

  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files)
  >  (dirs)
  >  (source_trees))
  > EOF

  $ dune build @install

  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/opam"
  ]
