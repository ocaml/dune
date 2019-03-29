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
