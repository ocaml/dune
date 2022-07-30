  $ dune build --profile=release --display short --debug-dependency-path @all
        coqdep bar/bar.v.d
        coqdep foo/foo.v.d
        coqdep foo/a/a.v.d
          coqc foo/Nfoo_foo.{cmi,cmxs},foo/foo.{glob,vo}
          coqc foo/a/Nfoo_a_a.{cmi,cmxs},foo/a/a.{glob,vo}
          coqc bar/Nbar_baz_bar.{cmi,cmxs},bar/bar.{glob,vo}

  $ dune build --profile=release --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmi" {"coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmi"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmxs" {"coq/user-contrib/bar/baz/.coq-native/Nbar_baz_bar.cmxs"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/bar.v" {"coq/user-contrib/bar/baz/bar.v"}
    "_build/install/default/lib/coq/user-contrib/bar/baz/bar.vo" {"coq/user-contrib/bar/baz/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs" {"coq/user-contrib/foo/.coq-native/Nfoo_foo.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmi" {"coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmi"}
    "_build/install/default/lib/coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmxs" {"coq/user-contrib/foo/a/.coq-native/Nfoo_a_a.cmxs"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.v" {"coq/user-contrib/foo/a/a.v"}
    "_build/install/default/lib/coq/user-contrib/foo/a/a.vo" {"coq/user-contrib/foo/a/a.vo"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.v" {"coq/user-contrib/foo/foo.v"}
    "_build/install/default/lib/coq/user-contrib/foo/foo.vo" {"coq/user-contrib/foo/foo.vo"}
  ]
