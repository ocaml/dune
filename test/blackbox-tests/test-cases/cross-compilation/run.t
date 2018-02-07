  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf $JBUILDER build --display short --root . -j1 -x foo file @install
      ocamldep bin/blah.ml.d [default.foo]
      ocamldep lib/p.ml.d [default.foo]
      ocamldep bin/blah.ml.d
        ocamlc lib/.p.objs/p.{cmi,cmo,cmt} [default.foo]
      ocamldep lib/p.ml.d
      ocamlopt lib/.p.objs/p.{cmx,o} [default.foo]
        ocamlc bin/.blah.eobjs/blah.{cmi,cmo,cmt} [default.foo]
        ocamlc lib/p.cma [default.foo]
        ocamlc lib/.p.objs/p.{cmi,cmo,cmt}
      ocamlopt lib/p.{a,cmxa} [default.foo]
      ocamlopt bin/.blah.eobjs/blah.{cmx,o} [default.foo]
        ocamlc bin/.blah.eobjs/blah.{cmi,cmo,cmt}
      ocamlopt lib/.p.objs/p.{cmx,o}
      ocamlopt lib/p.cmxs [default.foo]
      ocamlopt bin/blah.exe [default.foo]
      ocamlopt bin/.blah.eobjs/blah.{cmx,o}
      ocamlopt lib/p.{a,cmxa}
      ocamlopt bin/blah.exe
          blah file [default.foo]
          blah file
  $ cat _build/default.foo/file
  42
  $ ls *.install
  p-foo.install
  $ cat p-foo.install
  lib: [
    "_build/install/default.foo/lib/p/META" {"../../foo-sysroot/lib/p/META"}
    "_build/install/default.foo/lib/p/opam" {"../../foo-sysroot/lib/p/opam"}
    "_build/install/default.foo/lib/p/p.cmi" {"../../foo-sysroot/lib/p/p.cmi"}
    "_build/install/default.foo/lib/p/p.cmx" {"../../foo-sysroot/lib/p/p.cmx"}
    "_build/install/default.foo/lib/p/p.cmt" {"../../foo-sysroot/lib/p/p.cmt"}
    "_build/install/default.foo/lib/p/p.ml" {"../../foo-sysroot/lib/p/p.ml"}
    "_build/install/default.foo/lib/p/p.cma" {"../../foo-sysroot/lib/p/p.cma"}
    "_build/install/default.foo/lib/p/p.cmxa" {"../../foo-sysroot/lib/p/p.cmxa"}
    "_build/install/default.foo/lib/p/p.a" {"../../foo-sysroot/lib/p/p.a"}
    "_build/install/default.foo/lib/p/p.cmxs" {"../../foo-sysroot/lib/p/p.cmxs"}
  ]
  bin: [
    "_build/install/default.foo/bin/blah" {"../foo-sysroot/bin/blah"}
  ]
