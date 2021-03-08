  $ dune build --display short --debug-dependency-path
        coqdep a/a.v.d
        coqdep b/b.v.d
          coqc a/.a.aux,a/a.{glob,vo}
          coqc b/.b.aux,b/b.{glob,vo}
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
