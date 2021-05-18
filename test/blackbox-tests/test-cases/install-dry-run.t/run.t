  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ dune build @install
  $ dune install --dry-run 2>&1 | dune_cmd sanitize
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/META
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/META
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/META to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/META (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/dune-package
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/dune-package
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/dune-package to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/dune-package (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib$ext_lib
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib$ext_lib
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib$ext_lib to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib$ext_lib (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cma
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cma
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cma to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cma (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmi
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmi
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmi to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmi (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmt
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmt
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmt to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmt (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmx
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmx
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmx to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmx (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxa
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxa
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxa to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxa (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.ml
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.ml
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.ml to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.ml (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/opam
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/opam
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/opam to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/opam (executable: false)
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxs
  Installing /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxs
  Creating directory /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxs to /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxs (executable: true)

  $ dune uninstall --dry-run 2>&1 | dune_cmd sanitize
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/META
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/dune-package
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib$ext_lib
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cma
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmi
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmt
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmx
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxa
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.ml
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/opam
  Removing (if it exists) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib/mylib.cmxs
  Removing directory (if empty) /Users/rgrinberg/github/ocaml/dune/_opam/lib/mylib
