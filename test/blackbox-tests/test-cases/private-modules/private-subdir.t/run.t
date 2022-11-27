Private modules are not excluded from the install file, but installed in the
.private subdir

  $ dune build | grep -i priv
    "_build/install/default/lib/lib/.private/lib__Priv.cmi" {".private/lib__Priv.cmi"}
    "_build/install/default/lib/lib/.private/lib__Priv.cmt" {".private/lib__Priv.cmt"}
    "_build/install/default/lib/lib/foo/.private/priv2.cmi" {"foo/.private/priv2.cmi"}
    "_build/install/default/lib/lib/foo/.private/priv2.cmt" {"foo/.private/priv2.cmt"}
    "_build/install/default/lib/lib/foo/priv2.cmx" {"foo/priv2.cmx"}
    "_build/install/default/lib/lib/foo/priv2.ml" {"foo/priv2.ml"}
    "_build/install/default/lib/lib/lib__Priv.cmx"
    "_build/install/default/lib/lib/priv.ml"
