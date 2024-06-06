  $ dune build --display short --debug-dependency-path @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
        coqdep .rec_module.theory.d
          coqc b/foo.{glob,vo}
          coqc c/ooo.{glob,vo}
          coqc c/d/bar.{glob,vo}
          coqc a/bar.{glob,vo}

  $ dune build --debug-dependency-path @default
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  lib: [
    "_build/install/default/lib/rec/META"
    "_build/install/default/lib/rec/dune-package"
    "_build/install/default/lib/rec/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.glob" {"coq/user-contrib/rec_module/a/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.v" {"coq/user-contrib/rec_module/a/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/a/bar.vo" {"coq/user-contrib/rec_module/a/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.glob" {"coq/user-contrib/rec_module/b/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.v" {"coq/user-contrib/rec_module/b/foo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/b/foo.vo" {"coq/user-contrib/rec_module/b/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.glob" {"coq/user-contrib/rec_module/c/d/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.v" {"coq/user-contrib/rec_module/c/d/bar.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/d/bar.vo" {"coq/user-contrib/rec_module/c/d/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.glob" {"coq/user-contrib/rec_module/c/ooo.glob"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.v" {"coq/user-contrib/rec_module/c/ooo.v"}
    "_build/install/default/lib/coq/user-contrib/rec_module/c/ooo.vo" {"coq/user-contrib/rec_module/c/ooo.vo"}
  ]
