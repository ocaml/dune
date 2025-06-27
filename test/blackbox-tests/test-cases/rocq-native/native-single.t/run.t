  $ dune build --profile=release --display short --debug-dependency-path @all
  File "dune", line 4, characters 7-13:
  4 |  (mode native)
             ^^^^^^
  Warning: 'native' was deprecated in version 0.7 of Rocq Prover build
  language. Since Coq lang 0.7 native mode is automatically inferred from the
  configuration of Coq.
        coqdep .basic.theory.d
          coqc Nbasic_foo.{cmi,cmxs},foo.{glob,vo}
          coqc Nbasic_bar.{cmi,cmxs},bar.{glob,vo}

  $ dune build --profile=release --debug-dependency-path @default
  File "dune", line 4, characters 7-13:
  4 |  (mode native)
             ^^^^^^
  Warning: 'native' was deprecated in version 0.7 of Rocq Prover build
  language. Since Coq lang 0.7 native mode is automatically inferred from the
  configuration of Coq.
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/.coq-native/Nbasic_bar.cmi" {"coq/user-contrib/basic/.coq-native/Nbasic_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/basic/.coq-native/Nbasic_bar.cmxs" {"coq/user-contrib/basic/.coq-native/Nbasic_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/basic/.coq-native/Nbasic_foo.cmi" {"coq/user-contrib/basic/.coq-native/Nbasic_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/basic/.coq-native/Nbasic_foo.cmxs" {"coq/user-contrib/basic/.coq-native/Nbasic_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.glob" {"coq/user-contrib/basic/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.glob" {"coq/user-contrib/basic/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
  ]
