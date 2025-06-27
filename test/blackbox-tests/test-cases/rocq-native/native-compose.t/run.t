  $ dune build --profile=release --display short --debug-dependency-path @all
  File "foo/dune", line 4, characters 7-13:
  4 |  (mode native)
             ^^^^^^
  Warning: 'native' was deprecated in version 0.7 of Rocq Prover build
  language. Since Coq lang 0.7 native mode is automatically inferred from the
  configuration of Coq.
  File "bar/dune", line 4, characters 7-13:
  4 |  (mode native)
             ^^^^^^
  Warning: 'native' was deprecated in version 0.7 of Rocq Prover build
  language. Since Coq lang 0.7 native mode is automatically inferred from the
  configuration of Coq.
        coqdep bar/.bar.theory.d
        coqdep foo/.foo.theory.d
          coqc foo/Nfoo_foo.{cmi,cmxs},foo/foo.{glob,vo}
          coqc foo/a/Nfoo_a_a.{cmi,cmxs},foo/a/a.{glob,vo}
          coqc bar/Nbar_baz_bar.{cmi,cmxs},bar/bar.{glob,vo}

  $ dune build --profile=release --debug-dependency-path @default
  File "foo/dune", line 4, characters 7-13:
  4 |  (mode native)
             ^^^^^^
  Warning: 'native' was deprecated in version 0.7 of Rocq Prover build
  language. Since Coq lang 0.7 native mode is automatically inferred from the
  configuration of Coq.
  File "bar/dune", line 4, characters 7-13:
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
    "_build/install/default/lib/coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmi" {"coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmxs" {"coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/bar.glob" {"coq/user-contrib/bar/baz/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/bar.v" {"coq/user-contrib/bar/baz/bar.v"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/bar.vo" {"coq/user-contrib/bar/baz/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmi" {"coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmxs" {"coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.glob" {"coq/user-contrib/foo/a/a.glob"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.v" {"coq/user-contrib/foo/a/a.v"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.vo" {"coq/user-contrib/foo/a/a.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.glob" {"coq/user-contrib/foo/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.v" {"coq/user-contrib/foo/foo.v"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.vo" {"coq/user-contrib/foo/foo.vo"}
  ]
