  $ echo "(vendored_dirs a)" > dune
  $ dune build -p b @install
  $ dune install --prefix=./install b
  Installing install/lib/b/META
  Installing install/lib/b/b.a
  Installing install/lib/b/b.cma
  Installing install/lib/b/b.cmi
  Installing install/lib/b/b.cmt
  Installing install/lib/b/b.cmti
  Installing install/lib/b/b.cmx
  Installing install/lib/b/b.cmxa
  Installing install/lib/b/b.ml
  Installing install/lib/b/b.mli
  Installing install/lib/b/dune-package
  Installing install/lib/b/opam
  Installing install/lib/b/b.cmxs
  $ export OCAMLPATH="$PWD/install/lib":$OCAMLPATH
  $ echo "(data_only_dirs a b)" > dune
  $ dune build -p c
  File "$TESTCASE_ROOT/install/lib/b/dune-package", line 29, characters 11-12:
  29 |  (requires a)
                  ^
  Error: Library "a" not found.
  -> required by library "b" in
     $TESTCASE_ROOT/install/lib/b
  -> required by _build/default/META.c
  -> required by _build/install/default/lib/c/META
  -> required by _build/default/c.install
  -> required by alias install
  [1]
