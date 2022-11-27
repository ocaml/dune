install stanza is respected

  $ dune build
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/opam"
  ]
  share: [
    "_build/install/default/share/foo/foobar"
    "_build/install/default/share/foo/share1"
  ]
