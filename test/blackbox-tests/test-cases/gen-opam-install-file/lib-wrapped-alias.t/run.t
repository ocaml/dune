wrapped lib with lib interface module

  $ dune build | dune_cmd sanitize
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/bar.ml"
    "_build/install/default/lib/foo/bar.mli"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo$ext_lib"
    "_build/install/default/lib/foo/foo.cma"
    "_build/install/default/lib/foo/foo.cmi"
    "_build/install/default/lib/foo/foo.cmt"
    "_build/install/default/lib/foo/foo.cmx"
    "_build/install/default/lib/foo/foo.cmxa"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/foo__.cmi"
    "_build/install/default/lib/foo/foo__.cmt"
    "_build/install/default/lib/foo/foo__.cmx"
    "_build/install/default/lib/foo/foo__.ml"
    "_build/install/default/lib/foo/foo__Bar.cmi"
    "_build/install/default/lib/foo/foo__Bar.cmt"
    "_build/install/default/lib/foo/foo__Bar.cmti"
    "_build/install/default/lib/foo/foo__Bar.cmx"
    "_build/install/default/lib/foo/opam"
  ]
  libexec: [
    "_build/install/default/lib/foo/foo.cmxs"
  ]
