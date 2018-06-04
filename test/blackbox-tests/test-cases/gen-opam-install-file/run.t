  $ dune runtest --display short
      ocamldep bar.ml.d
      ocamldep foo.ml.d
      ocamldep foo.mli.d
        ocamlc .foo.objs/foo.{cmi,cmti}
      ocamlopt .foo.objs/foo.{cmx,o}
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs
      ocamldep foo_byte.ml.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
        ocamlc foo_byte.cma
      ocamldep ppx-old/foo_ppx_rewriter.ml.d
        ocamlc ppx-old/.foo_ppx_rewriter.objs/foo_ppx_rewriter.{cmi,cmo,cmt}
      ocamlopt ppx-old/.foo_ppx_rewriter.objs/foo_ppx_rewriter.{cmx,o}
      ocamlopt ppx-old/foo_ppx_rewriter.{a,cmxa}
      ocamlopt ppx-old/foo_ppx_rewriter.cmxs
        ocamlc .foo.objs/foo.{cmo,cmt}
        ocamlc foo.cma
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt bar.exe
        ocamlc ppx-old/foo_ppx_rewriter.cma
      ocamlopt .ppx/foo.ppx_rewriter/ppx.exe
  lib: [
    "_build/install/default/lib/foo/META" {"META"}
    "_build/install/default/lib/foo/opam" {"opam"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cmi" {"ppx_rewriter/foo_ppx_rewriter.cmi"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cmx" {"ppx_rewriter/foo_ppx_rewriter.cmx"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cmt" {"ppx_rewriter/foo_ppx_rewriter.cmt"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.ml" {"ppx_rewriter/foo_ppx_rewriter.ml"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cma" {"ppx_rewriter/foo_ppx_rewriter.cma"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cmxa" {"ppx_rewriter/foo_ppx_rewriter.cmxa"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter$ext_lib" {"ppx_rewriter/foo_ppx_rewriter$ext_lib"}
    "_build/install/default/lib/foo/ppx_rewriter/foo_ppx_rewriter.cmxs" {"ppx_rewriter/foo_ppx_rewriter.cmxs"}
    "_build/install/default/lib/foo/ppx_rewriter/foo.ppx_rewriter.dune" {"ppx_rewriter/foo.ppx_rewriter.dune"}
    "_build/install/default/lib/foo/foo.cmi" {"foo.cmi"}
    "_build/install/default/lib/foo/foo.cmx" {"foo.cmx"}
    "_build/install/default/lib/foo/foo.cmt" {"foo.cmt"}
    "_build/install/default/lib/foo/foo.cmti" {"foo.cmti"}
    "_build/install/default/lib/foo/foo.mli" {"foo.mli"}
    "_build/install/default/lib/foo/foo.ml" {"foo.ml"}
    "_build/install/default/lib/foo/foo.cma" {"foo.cma"}
    "_build/install/default/lib/foo/foo.cmxa" {"foo.cmxa"}
    "_build/install/default/lib/foo/foo$ext_lib" {"foo$ext_lib"}
    "_build/install/default/lib/foo/foo.cmxs" {"foo.cmxs"}
    "_build/install/default/lib/foo/foo.js" {"foo.js"}
    "_build/install/default/lib/foo/cfoo.h" {"cfoo.h"}
    "_build/install/default/lib/foo/foo.dune" {"foo.dune"}
    "_build/install/default/lib/foo/byte/foo_byte.cmi" {"byte/foo_byte.cmi"}
    "_build/install/default/lib/foo/byte/foo_byte.cmt" {"byte/foo_byte.cmt"}
    "_build/install/default/lib/foo/byte/foo_byte.ml" {"byte/foo_byte.ml"}
    "_build/install/default/lib/foo/byte/foo_byte.cma" {"byte/foo_byte.cma"}
    "_build/install/default/lib/foo/byte/foo.byte.dune" {"byte/foo.byte.dune"}
  ]
  libexec: [
    "_build/install/default/lib/foo/ppx_rewriter/ppx.exe" {"ppx_rewriter/ppx.exe"}
  ]
  bin: [
    "_build/install/default/bin/bar" {"bar"}
  ]
  share: [
    "_build/install/default/share/foo/bar.ml"
    "_build/install/default/share/foo/baz.ml" {"baz.ml"}
  ]
  doc: [
    "_build/install/default/doc/foo/odoc-pages/doc.mld" {"odoc-pages/doc.mld"}
  ]
