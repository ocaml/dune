stubs and js files installed

  $ dune build | dune_cmd sanitize
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/cfoo.h"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo$ext_lib"
    "_build/install/default/lib/foo/foo.cma"
    "_build/install/default/lib/foo/foo.cmi"
    "_build/install/default/lib/foo/foo.cmt"
    "_build/install/default/lib/foo/foo.cmx"
    "_build/install/default/lib/foo/foo.cmxa"
    "_build/install/default/lib/foo/foo.js"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/libfoo_stubs$ext_lib"
    "_build/install/default/lib/foo/opam"
  ]
  libexec: [
    "_build/install/default/lib/foo/foo.cmxs"
  ]
  stublibs: [
    "_build/install/default/lib/stublibs/dllfoo_stubs$ext_dll"
  ]

