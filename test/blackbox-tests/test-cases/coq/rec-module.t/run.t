  $ dune build --display short --debug-dependency-path @all
        coqdep a/bar.v.d
        coqdep b/foo.v.d
        coqdep c/d/bar.v.d
        coqdep c/ooo.v.d
          coqc b/foo.{glob,vo}
          coqc c/d/bar.{glob,vo}
          coqc c/ooo.{glob,vo}
          coqc a/bar.{glob,vo}

  $ dune build --debug-dependency-path @default
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
