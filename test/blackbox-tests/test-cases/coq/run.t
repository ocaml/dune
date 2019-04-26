  $ dune build --root base --display short --debug-dependency-path @all
  Entering directory 'base'
        coqdep bar.v.d
        coqdep foo.v.d
          coqc foo.vo
          coqc bar.vo

  $ dune build --root rec_module --display short --debug-dependency-path @all
  Entering directory 'rec_module'
        coqdep a/bar.v.d
        coqdep b/foo.v.d
          coqc b/foo.vo
        coqdep c/d/bar.v.d
          coqc c/d/bar.vo
        coqdep c/ooo.v.d
          coqc c/ooo.vo
          coqc a/bar.vo

  $ dune build --root ml_lib --display short --debug-dependency-path @all
  Entering directory 'ml_lib'
         coqpp src_a/gram.ml
      ocamldep src_a/.ml_plugin_a.objs/gram.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a.{cmx,o}
      ocamldep src_a/.ml_plugin_a.objs/gram.mli.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmi,cmti}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmo,cmt}
      ocamldep src_a/.ml_plugin_a.objs/simple.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Simple.{cmi,cmo,cmt}
        ocamlc src_a/ml_plugin_a.cma
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b.{cmi,cmo,cmt}
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b.{cmx,o}
      ocamldep src_b/.ml_plugin_b.objs/simple_b.ml.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b__Simple_b.{cmi,cmo,cmt}
        ocamlc src_b/ml_plugin_b.cma
        coqdep theories/a.v.d
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Gram.{cmx,o}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Simple.{cmx,o}
      ocamlopt src_a/ml_plugin_a.{a,cmxa}
      ocamlopt src_a/ml_plugin_a.cmxs
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b__Simple_b.{cmx,o}
      ocamlopt src_b/ml_plugin_b.{a,cmxa}
      ocamlopt src_b/ml_plugin_b.cmxs
          coqc theories/a.vo

  $ dune build --root base --display short --debug-dependency-path @default
  Entering directory 'base'
  lib: [
    "_build/install/default/lib/base/META" {"META"}
    "_build/install/default/lib/base/dune-package" {"dune-package"}
    "_build/install/default/lib/base/opam" {"opam"}
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
  ]

  $ dune build --root rec_module --display short --debug-dependency-path @default
  Entering directory 'rec_module'
  lib: [
    "_build/install/default/lib/rec/META" {"META"}
    "_build/install/default/lib/rec/dune-package" {"dune-package"}
    "_build/install/default/lib/rec/opam" {"opam"}
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vo" {"coq/user-contrib/rec_module/a/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vo" {"coq/user-contrib/rec_module/b/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vo" {"coq/user-contrib/rec_module/c/d/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vo" {"coq/user-contrib/rec_module/c/ooo.vo"}
  ]
