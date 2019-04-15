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
         coqpp src/gram.ml
      ocamldep src/.ml_plugin.objs/gram.ml.d
        ocamlc src/.ml_plugin.objs/byte/ml_plugin.{cmi,cmo,cmt}
      ocamlopt src/.ml_plugin.objs/native/ml_plugin.{cmx,o}
      ocamldep src/.ml_plugin.objs/gram.mli.d
        ocamlc src/.ml_plugin.objs/byte/ml_plugin__Gram.{cmi,cmti}
        ocamlc src/.ml_plugin.objs/byte/ml_plugin__Gram.{cmo,cmt}
      ocamldep src/.ml_plugin.objs/simple.ml.d
        ocamlc src/.ml_plugin.objs/byte/ml_plugin__Simple.{cmi,cmo,cmt}
        ocamlc src/ml_plugin.cma
        coqdep theories/a.v.d
      ocamlopt src/.ml_plugin.objs/native/ml_plugin__Gram.{cmx,o}
      ocamlopt src/.ml_plugin.objs/native/ml_plugin__Simple.{cmx,o}
      ocamlopt src/ml_plugin.{a,cmxa}
      ocamlopt src/ml_plugin.cmxs
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
