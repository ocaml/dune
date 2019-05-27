Odig files should be detected relative to the package directory
  $ dune build @install --root .
  $ cat project/foobar.install
  lib: [
    "_build/install/default/lib/foobar/META"
    "_build/install/default/lib/foobar/dune-package"
  ]
  doc: [
    "_build/install/default/doc/foobar/LICENSE"
  ]
