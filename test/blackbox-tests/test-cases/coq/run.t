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
        coqdep theories/a.v.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b.{cmi,cmo,cmt}
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b.{cmx,o}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a.{cmx,o}
      ocamldep src_a/.ml_plugin_a.objs/gram.mli.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmi,cmti}
      ocamldep src_a/.ml_plugin_a.objs/simple.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Simple.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Simple.{cmx,o}
      ocamldep src_b/.ml_plugin_b.objs/simple_b.ml.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b__Simple_b.{cmi,cmo,cmt}
        ocamlc src_b/ml_plugin_b.cma
         coqpp src_a/gram.ml
      ocamldep src_a/.ml_plugin_a.objs/gram.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmo,cmt}
        ocamlc src_a/ml_plugin_a.cma
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b__Simple_b.{cmx,o}
      ocamlopt src_b/ml_plugin_b.{a,cmxa}
      ocamlopt src_b/ml_plugin_b.cmxs
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Gram.{cmx,o}
      ocamlopt src_a/ml_plugin_a.{a,cmxa}
      ocamlopt src_a/ml_plugin_a.cmxs
          coqc theories/a.vo

  $ dune build --root base --display short --debug-dependency-path @default
  Entering directory 'base'
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
  ]

  $ dune build --root rec_module --display short --debug-dependency-path @default
  Entering directory 'rec_module'
  lib: [
    "_build/install/default/lib/rec/META"
    "_build/install/default/lib/rec/dune-package"
    "_build/install/default/lib/rec/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.v" {"coq/user-contrib/rec_module/a/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vo" {"coq/user-contrib/rec_module/a/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.v" {"coq/user-contrib/rec_module/b/foo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vo" {"coq/user-contrib/rec_module/b/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.v" {"coq/user-contrib/rec_module/c/d/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vo" {"coq/user-contrib/rec_module/c/d/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.v" {"coq/user-contrib/rec_module/c/ooo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vo" {"coq/user-contrib/rec_module/c/ooo.vo"}
  ]

  $ dune build --root compose_simple/ --display short --debug-dependency-path
  Entering directory 'compose_simple'
        coqdep a/a.v.d
          coqc a/a.vo
        coqdep b/b.v.d
          coqc b/b.vo
  lib: [
    "_build/install/default/lib/csimple/META"
    "_build/install/default/lib/csimple/dune-package"
    "_build/install/default/lib/csimple/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/a/a.v" {"coq/user-contrib/a/a.v"}
    "_build/install/default/lib/coq/user-contrib/a/a.vo" {"coq/user-contrib/a/a.vo"}
    "_build/install/default/lib/coq/user-contrib/b/b.v" {"coq/user-contrib/b/b.v"}
    "_build/install/default/lib/coq/user-contrib/b/b.vo" {"coq/user-contrib/b/b.vo"}
  ]

  $ dune build --root compose_plugin --display short --debug-dependency-path @all
  Entering directory 'compose_plugin'
        coqdep thy1/a.v.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b.{cmi,cmo,cmt}
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b.{cmx,o}
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a.{cmx,o}
      ocamldep src_a/.ml_plugin_a.objs/gram.mli.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmi,cmti}
      ocamldep src_a/.ml_plugin_a.objs/simple.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Simple.{cmi,cmo,cmt}
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Simple.{cmx,o}
      ocamldep src_b/.ml_plugin_b.objs/simple_b.ml.d
        ocamlc src_b/.ml_plugin_b.objs/byte/ml_plugin_b__Simple_b.{cmi,cmo,cmt}
        ocamlc src_b/ml_plugin_b.cma
        coqdep thy2/a.v.d
         coqpp src_a/gram.ml
      ocamldep src_a/.ml_plugin_a.objs/gram.ml.d
        ocamlc src_a/.ml_plugin_a.objs/byte/ml_plugin_a__Gram.{cmo,cmt}
        ocamlc src_a/ml_plugin_a.cma
      ocamlopt src_b/.ml_plugin_b.objs/native/ml_plugin_b__Simple_b.{cmx,o}
      ocamlopt src_b/ml_plugin_b.{a,cmxa}
      ocamlopt src_b/ml_plugin_b.cmxs
      ocamlopt src_a/.ml_plugin_a.objs/native/ml_plugin_a__Gram.{cmx,o}
      ocamlopt src_a/ml_plugin_a.{a,cmxa}
      ocamlopt src_a/ml_plugin_a.cmxs
          coqc thy1/a.vo
          coqc thy2/a.vo

# Test only works on Coq 8.12 due to boot constraints
# $ dune build --root compose_boot/ --display short --debug-dependency-path

  $ dune build --root public_dep_on_private/ --display short --debug-dependency-path
  Entering directory 'public_dep_on_private'
  File "public/dune", line 4, characters 11-18:
  4 |  (theories private))
                 ^^^^^^^
  Error: Theory "private" is private, it cannot be a dependency of a public
  theory. You need to associate "private" to a package.
  -> required by public/b.v.d
  -> required by public/b.vo
  -> required by install lib/coq/user-contrib/public/b.vo
  -> required by public.install
  -> required by alias default
  -> required by alias default
  [1]

  $ dune build --root compose_cycle/ --display short --debug-dependency-path
  Entering directory 'compose_cycle'
  File "a/dune", line 2, characters 7-8:
  2 |  (name a)
             ^
  Error: Cycle found
  - b
  - a
  - b
  -> required by a/a.v.d
  -> required by a/a.vo
  -> required by install lib/coq/user-contrib/a/a.vo
  -> required by ccycle.install
  -> required by alias default
  -> required by alias default
  File "b/dune", line 2, characters 7-8:
  2 |  (name b)
             ^
  Error: Cycle found
  - a
  - b
  - a
  -> required by b/b.v.d
  -> required by b/b.vo
  -> required by install lib/coq/user-contrib/b/b.vo
  -> required by ccycle.install
  -> required by alias default
  -> required by alias default
  [1]

  $ dune build --root compose_sub_theory/ --display short --debug-dependency-path
  Entering directory 'compose_sub_theory'
        coqdep b/b.v.d
        coqdep a/a.v.d
          coqc a/a.vo
          coqc b/b.vo
  lib: [
    "_build/install/default/lib/subtheory/META"
    "_build/install/default/lib/subtheory/dune-package"
    "_build/install/default/lib/subtheory/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/b/b.v" {"coq/user-contrib/b/b.v"}
    "_build/install/default/lib/coq/user-contrib/b/b.vo" {"coq/user-contrib/b/b.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.v" {"coq/user-contrib/foo/a/a.v"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.vo" {"coq/user-contrib/foo/a/a.vo"}
  ]

  $ dune build --root compose_two_scopes/ --display short --debug-dependency-path
  Entering directory 'compose_two_scopes'
  File "b/dune", line 4, characters 11-12:
  4 |  (theories a))
                 ^
  Error: Theory a not found
  -> required by b/b.v.d
  -> required by b/b.vo
  -> required by install lib/coq/user-contrib/b/b.vo
  -> required by cvendor.install
  -> required by alias default
  -> required by alias default
  [1]
