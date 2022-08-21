byte code only library

  $ dune build
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/byte/foo_byte.cma" {"byte/foo_byte.cma"}
    "_build/install/default/lib/foo/byte/foo_byte.cmi" {"byte/foo_byte.cmi"}
    "_build/install/default/lib/foo/byte/foo_byte.cmt" {"byte/foo_byte.cmt"}
    "_build/install/default/lib/foo/byte/foo_byte.ml" {"byte/foo_byte.ml"}
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/opam"
  ]
