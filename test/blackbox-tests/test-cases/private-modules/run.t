  $ dune build --root accessible-via-public
  Entering directory 'accessible-via-public'
        runfoo alias default
  private module bar

  $ dune build --root inaccessible-in-deps 2>&1 | grep -v "cd _build"
  Entering directory 'inaccessible-in-deps'
        ocamlc .foo.eobjs/foo.{cmi,cmo,cmt} (exit 2)
  File "foo.ml", line 1, characters 0-5:
  Error: Unbound module X

  $ dune build --root excluded-from-install-file
  Entering directory 'excluded-from-install-file'
  lib: [
    "_build/install/default/lib/lib/META" {"META"}
    "_build/install/default/lib/lib/foo.ml" {"foo.ml"}
    "_build/install/default/lib/lib/foo/bar.cmi" {"foo/bar.cmi"}
    "_build/install/default/lib/lib/foo/bar.cmt" {"foo/bar.cmt"}
    "_build/install/default/lib/lib/foo/bar.cmx" {"foo/bar.cmx"}
    "_build/install/default/lib/lib/foo/bar.ml" {"foo/bar.ml"}
    "_build/install/default/lib/lib/foo/lib.foo.dune" {"foo/lib.foo.dune"}
    "_build/install/default/lib/lib/foo/lib_foo$ext_lib" {"foo/lib_foo$ext_lib"}
    "_build/install/default/lib/lib/foo/lib_foo.cma" {"foo/lib_foo.cma"}
    "_build/install/default/lib/lib/foo/lib_foo.cmxa" {"foo/lib_foo.cmxa"}
    "_build/install/default/lib/lib/foo/lib_foo.cmxs" {"foo/lib_foo.cmxs"}
    "_build/install/default/lib/lib/foo/priv2.cmt" {"foo/priv2.cmt"}
    "_build/install/default/lib/lib/foo/priv2.cmx" {"foo/priv2.cmx"}
    "_build/install/default/lib/lib/foo/priv2.ml" {"foo/priv2.ml"}
    "_build/install/default/lib/lib/lib$ext_lib" {"lib$ext_lib"}
    "_build/install/default/lib/lib/lib.cma" {"lib.cma"}
    "_build/install/default/lib/lib/lib.cmi" {"lib.cmi"}
    "_build/install/default/lib/lib/lib.cmt" {"lib.cmt"}
    "_build/install/default/lib/lib/lib.cmx" {"lib.cmx"}
    "_build/install/default/lib/lib/lib.cmxa" {"lib.cmxa"}
    "_build/install/default/lib/lib/lib.cmxs" {"lib.cmxs"}
    "_build/install/default/lib/lib/lib.dune" {"lib.dune"}
    "_build/install/default/lib/lib/lib.ml-gen" {"lib.ml-gen"}
    "_build/install/default/lib/lib/lib__Foo.cmi" {"lib__Foo.cmi"}
    "_build/install/default/lib/lib/lib__Foo.cmt" {"lib__Foo.cmt"}
    "_build/install/default/lib/lib/lib__Foo.cmx" {"lib__Foo.cmx"}
    "_build/install/default/lib/lib/lib__Priv.cmt" {"lib__Priv.cmt"}
    "_build/install/default/lib/lib/lib__Priv.cmx" {"lib__Priv.cmx"}
    "_build/install/default/lib/lib/opam" {"opam"}
    "_build/install/default/lib/lib/priv.ml" {"priv.ml"}
  ]
