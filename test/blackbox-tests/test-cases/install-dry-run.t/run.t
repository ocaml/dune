  $ dune build @install
  $ dune install --dry-run 2>&1 | sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  Installing OPAM_PREFIX/lib/mylib/META
  Installing OPAM_PREFIX/lib/mylib/dune-package
  Installing OPAM_PREFIX/lib/mylib/mylib$ext_lib
  Installing OPAM_PREFIX/lib/mylib/mylib.cma
  Installing OPAM_PREFIX/lib/mylib/mylib.cmi
  Installing OPAM_PREFIX/lib/mylib/mylib.cmt
  Installing OPAM_PREFIX/lib/mylib/mylib.cmx
  Installing OPAM_PREFIX/lib/mylib/mylib.cmxa
  Installing OPAM_PREFIX/lib/mylib/mylib.cmxs
  Installing OPAM_PREFIX/lib/mylib/mylib.ml
  Installing OPAM_PREFIX/lib/mylib/opam
  Removing (if it exists) OPAM_PREFIX/lib/mylib/META
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/META to OPAM_PREFIX/lib/mylib/META (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/dune-package
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/dune-package to OPAM_PREFIX/lib/mylib/dune-package (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib$ext_lib
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib$ext_lib to OPAM_PREFIX/lib/mylib/mylib$ext_lib (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cma
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cma to OPAM_PREFIX/lib/mylib/mylib.cma (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmi
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmi to OPAM_PREFIX/lib/mylib/mylib.cmi (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmt
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmt to OPAM_PREFIX/lib/mylib/mylib.cmt (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmx
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmx to OPAM_PREFIX/lib/mylib/mylib.cmx (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmxa
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxa to OPAM_PREFIX/lib/mylib/mylib.cmxa (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmxs
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.cmxs to OPAM_PREFIX/lib/mylib/mylib.cmxs (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.ml
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/mylib.ml to OPAM_PREFIX/lib/mylib/mylib.ml (executable: false)
  Removing (if it exists) OPAM_PREFIX/lib/mylib/opam
  Creating directory OPAM_PREFIX/lib/mylib
  Copying _build/install/default/lib/mylib/opam to OPAM_PREFIX/lib/mylib/opam (executable: false)

  $ dune uninstall --dry-run 2>&1 | sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  Removing (if it exists) OPAM_PREFIX/lib/mylib/META
  Removing (if it exists) OPAM_PREFIX/lib/mylib/dune-package
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib$ext_lib
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cma
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmi
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmt
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmx
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmxa
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.cmxs
  Removing (if it exists) OPAM_PREFIX/lib/mylib/mylib.ml
  Removing (if it exists) OPAM_PREFIX/lib/mylib/opam
  Removing directory (if empty) OPAM_PREFIX/lib/mylib
