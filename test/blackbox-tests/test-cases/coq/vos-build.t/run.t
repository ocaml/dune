  $ dune build --display short foo.vos
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .basic.theory.d
          coqc foo.vos

  $ dune clean
  $ dune build --display short bar.vos
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .basic.theory.d
          coqc foo.vos
          coqc bar.vos

  $ cat foo.v | dune coq top -- foo.v 2>/dev/null | sed '/^Welcome to Coq/d'
  mynat is defined
  $ cat bar.v | dune coq top -- bar.v 2>/dev/null | sed '/^Welcome to Coq/d'
  mynum is defined

  $ dune clean
  $ dune build --display short --debug-dependency-path @all
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .basic.theory.d
          coqc foo.vos
          coqc bar.vos
  $ dune build --debug-dependency-path @default
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vos" {"coq/user-contrib/basic/bar.vos"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vos" {"coq/user-contrib/basic/foo.vos"}
    "_build/install/default/lib/coq/user-contrib/basic/rocq-package" {"coq/user-contrib/basic/rocq-package"}
  ]

Checking that we can go back to vo mode (without cleaning).

  $ mv dune-vo dune
  $ dune build --display short --debug-dependency-path @all
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
          coqc foo.{glob,vo}
          coqc bar.{glob,vo}
  $ dune build --debug-dependency-path @default
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.glob" {"coq/user-contrib/basic/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.glob" {"coq/user-contrib/basic/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/rocq-package" {"coq/user-contrib/basic/rocq-package"}
  ]
