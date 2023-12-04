Installing a library with `-x foo` should install the library for that context

  $ mkdir prefix
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf


  $ dune build @install -x foo

  $ dune install --dry-run --prefix prefix --display short -p repro -x foo 2>&1 | grep "Installing"
  Installing prefix/foo-sysroot/lib/repro/META
  Installing prefix/foo-sysroot/lib/repro/dune-package
  Installing prefix/foo-sysroot/lib/repro/foo.ml
  Installing prefix/foo-sysroot/lib/repro/repro.a
  Installing prefix/foo-sysroot/lib/repro/repro.cma
  Installing prefix/foo-sysroot/lib/repro/repro.cmi
  Installing prefix/foo-sysroot/lib/repro/repro.cmt
  Installing prefix/foo-sysroot/lib/repro/repro.cmx
  Installing prefix/foo-sysroot/lib/repro/repro.cmxa
  Installing prefix/foo-sysroot/lib/repro/repro.ml
  Installing prefix/foo-sysroot/lib/repro/repro__Foo.cmi
  Installing prefix/foo-sysroot/lib/repro/repro__Foo.cmt
  Installing prefix/foo-sysroot/lib/repro/repro__Foo.cmx
  Installing prefix/foo-sysroot/lib/repro/repro.cmxs
