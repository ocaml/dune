  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf dune build -x foo file @install --promote-install-files
  $ cat _build/default.foo/file
  42
  $ ls *.install
  p-foo.install
  $ dune_cmd cat p-foo.install | dune_cmd sanitize
  lib: [
    "_build/install/default.foo/lib/p/META" {"../../foo-sysroot/lib/p/META"}
    "_build/install/default.foo/lib/p/dune-package" {"../../foo-sysroot/lib/p/dune-package"}
    "_build/install/default.foo/lib/p/opam" {"../../foo-sysroot/lib/p/opam"}
    "_build/install/default.foo/lib/p/p$ext_lib" {"../../foo-sysroot/lib/p/p$ext_lib"}
    "_build/install/default.foo/lib/p/p.cma" {"../../foo-sysroot/lib/p/p.cma"}
    "_build/install/default.foo/lib/p/p.cmi" {"../../foo-sysroot/lib/p/p.cmi"}
    "_build/install/default.foo/lib/p/p.cmt" {"../../foo-sysroot/lib/p/p.cmt"}
    "_build/install/default.foo/lib/p/p.cmx" {"../../foo-sysroot/lib/p/p.cmx"}
    "_build/install/default.foo/lib/p/p.cmxa" {"../../foo-sysroot/lib/p/p.cmxa"}
    "_build/install/default.foo/lib/p/p.ml" {"../../foo-sysroot/lib/p/p.ml"}
  ]
  libexec: [
    "_build/install/default.foo/lib/p/p.cmxs" {"../../foo-sysroot/lib/p/p.cmxs"}
  ]
  bin: [
    "_build/install/default.foo/bin/blah" {"../foo-sysroot/bin/blah"}
  ]
