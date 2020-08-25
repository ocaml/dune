  $ dune build --display short --debug-dependency-path
        coqdep b/b.v.d
        coqdep a/a.v.d
          coqc a/.a.aux,a/a.{glob,vo}
          coqc b/.b.aux,b/b.{glob,vo}
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
