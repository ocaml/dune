  $ dune build --display short @all
          rocq .basic.theory.d
          rocq foo.{glob,vo}
  File "./foo.v", line 4, characters 0-19:
  Warning: Use of "Variable" or "Hypothesis" outside sections behaves as
  "#[local] Parameter" or "#[local] Axiom".
  [declaration-outside-section,vernacular,default]
          rocq bar.{glob,vo}

  $ dune build --debug-dependency-path @default
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
