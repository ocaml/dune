  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ dune build @install
  $ dune install --dry-run --display short 2>&1 --prefix _install | dune_cmd sanitize
  Removing (if it exists) _install/lib/mylib/META
  Installing _install/lib/mylib/META
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/META to _install/lib/mylib/META (executable: false)
  Removing (if it exists) _install/lib/mylib/dune-package
  Installing _install/lib/mylib/dune-package
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/dune-package to _install/lib/mylib/dune-package (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib$ext_lib
  Installing _install/lib/mylib/mylib$ext_lib
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib$ext_lib to _install/lib/mylib/mylib$ext_lib (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cma
  Installing _install/lib/mylib/mylib.cma
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cma to _install/lib/mylib/mylib.cma (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cmi
  Installing _install/lib/mylib/mylib.cmi
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmi to _install/lib/mylib/mylib.cmi (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cmt
  Installing _install/lib/mylib/mylib.cmt
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmt to _install/lib/mylib/mylib.cmt (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cmx
  Installing _install/lib/mylib/mylib.cmx
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmx to _install/lib/mylib/mylib.cmx (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cmxa
  Installing _install/lib/mylib/mylib.cmxa
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxa to _install/lib/mylib/mylib.cmxa (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.ml
  Installing _install/lib/mylib/mylib.ml
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.ml to _install/lib/mylib/mylib.ml (executable: false)
  Removing (if it exists) _install/lib/mylib/opam
  Installing _install/lib/mylib/opam
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/opam to _install/lib/mylib/opam (executable: false)
  Removing (if it exists) _install/lib/mylib/mylib.cmxs
  Installing _install/lib/mylib/mylib.cmxs
  Creating directory _install/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxs to _install/lib/mylib/mylib.cmxs (executable: true)

  $ dune uninstall --dry-run --prefix _install --display short 2>&1 | dune_cmd sanitize
  Removing (if it exists) _install/lib/mylib/META
  Removing (if it exists) _install/lib/mylib/dune-package
  Removing (if it exists) _install/lib/mylib/mylib$ext_lib
  Removing (if it exists) _install/lib/mylib/mylib.cma
  Removing (if it exists) _install/lib/mylib/mylib.cmi
  Removing (if it exists) _install/lib/mylib/mylib.cmt
  Removing (if it exists) _install/lib/mylib/mylib.cmx
  Removing (if it exists) _install/lib/mylib/mylib.cmxa
  Removing (if it exists) _install/lib/mylib/mylib.ml
  Removing (if it exists) _install/lib/mylib/opam
  Removing (if it exists) _install/lib/mylib/mylib.cmxs
  Removing directory (warn if not empty) _install/lib/mylib
