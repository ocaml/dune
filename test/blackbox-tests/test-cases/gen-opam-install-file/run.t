  $ $JBUILDER runtest -j1 --display short --root .
      ocamldep bar.ml.d
      ocamldep foo_byte.ml.d
      ocamldep foo.ml.d
      ocamldep foo.mli.d
        ocamlc .foo_byte.objs/foo_byte.{cmi,cmo,cmt}
        ocamlc .foo.objs/foo.{cmi,cmti}
        ocamlc foo_byte.cma
        ocamlc .foo.objs/foo.{cmo,cmt}
      ocamlopt .foo.objs/foo.{cmx,o}
        ocamlc .bar.eobjs/bar.{cmi,cmo,cmt}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa}
      ocamlopt .bar.eobjs/bar.{cmx,o}
      ocamlopt foo.cmxs
      ocamlopt bar.exe
  lib: [
    "_build/install/default/lib/foo/META" {"META"}
    "_build/install/default/lib/foo/opam" {"opam"}
    "_build/install/default/lib/foo/foo.cmi"
    "_build/install/default/lib/foo/foo.cmx"
    "_build/install/default/lib/foo/foo.cmt"
    "_build/install/default/lib/foo/foo.cmti"
    "_build/install/default/lib/foo/foo.mli"
    "_build/install/default/lib/foo/foo.cma"
    "_build/install/default/lib/foo/foo.cmxa"
    "_build/install/default/lib/foo/foo.a"
    "_build/install/default/lib/foo/foo.cmxs"
    "_build/install/default/lib/foo/foo.js"
    "_build/install/default/lib/foo/cfoo.h"
    "_build/install/default/lib/foo/byte/foo_byte.cmi" {"byte/foo_byte.cmi"}
    "_build/install/default/lib/foo/byte/foo_byte.cmt" {"byte/foo_byte.cmt"}
    "_build/install/default/lib/foo/byte/foo_byte.ml" {"byte/foo_byte.ml"}
    "_build/install/default/lib/foo/byte/foo_byte.cma" {"byte/foo_byte.cma"}
  ]
  bin: [
    "_build/install/default/bin/bar" {"bar"}
  ]
  share: [
    "_build/install/default/share/foo/bar.ml"
    "_build/install/default/share/foo/baz.ml" {"baz.ml"}
  ]
