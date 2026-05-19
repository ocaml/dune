Installing a library with `-x foo` should install the library for that context

  $ mkdir prefix
  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf


  $ dune build @install -x foo
  $ dune_cmd cat _build/default.foo/repro-foo.install | dune_cmd sanitize
  lib: [
    "_build/install/default.foo/lib/repro/META" {"../../foo-sysroot/lib/repro/META"}
    "_build/install/default.foo/lib/repro/dune-package" {"../../foo-sysroot/lib/repro/dune-package"}
    "_build/install/default.foo/lib/repro/foo.ml" {"../../foo-sysroot/lib/repro/foo.ml"}
    "_build/install/default.foo/lib/repro/repro$ext_lib" {"../../foo-sysroot/lib/repro/repro$ext_lib"}
    "_build/install/default.foo/lib/repro/repro.cma" {"../../foo-sysroot/lib/repro/repro.cma"}
    "_build/install/default.foo/lib/repro/repro.cmi" {"../../foo-sysroot/lib/repro/repro.cmi"}
    "_build/install/default.foo/lib/repro/repro.cmt" {"../../foo-sysroot/lib/repro/repro.cmt"}
    "_build/install/default.foo/lib/repro/repro.cmx" {"../../foo-sysroot/lib/repro/repro.cmx"}
    "_build/install/default.foo/lib/repro/repro.cmxa" {"../../foo-sysroot/lib/repro/repro.cmxa"}
    "_build/install/default.foo/lib/repro/repro.ml" {"../../foo-sysroot/lib/repro/repro.ml"}
    "_build/install/default.foo/lib/repro/repro__Foo.cmi" {"../../foo-sysroot/lib/repro/repro__Foo.cmi"}
    "_build/install/default.foo/lib/repro/repro__Foo.cmt" {"../../foo-sysroot/lib/repro/repro__Foo.cmt"}
    "_build/install/default.foo/lib/repro/repro__Foo.cmx" {"../../foo-sysroot/lib/repro/repro__Foo.cmx"}
  ]
  libexec: [
    "_build/install/default.foo/lib/repro/repro.cmxs" {"../../foo-sysroot/lib/repro/repro.cmxs"}
  ]

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
