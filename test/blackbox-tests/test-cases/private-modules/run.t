  $ dune build --root accessible-via-public
  Entering directory 'accessible-via-public'
        runfoo alias default
  private module bar

  $ dune build --root inaccessible-in-deps 2>&1
  Entering directory 'inaccessible-in-deps'
  File "foo.ml", line 1, characters 0-5:
  1 | X.run ();;
      ^^^^^
  Error: Unbound module X
  [1]

Private modules are not excluded from the install file, but installed in the .private subdir
  $ dune build --root private-subdir | grep -i priv
  Entering directory 'private-subdir'
    "_build/install/default/lib/lib/.private/lib__Priv.cmi" {".private/lib__Priv.cmi"}
    "_build/install/default/lib/lib/foo/.private/priv2.cmi" {"foo/.private/priv2.cmi"}
    "_build/install/default/lib/lib/foo/priv2.cmt" {"foo/priv2.cmt"}
    "_build/install/default/lib/lib/foo/priv2.cmx" {"foo/priv2.cmx"}
    "_build/install/default/lib/lib/foo/priv2.ml" {"foo/priv2.ml"}
    "_build/install/default/lib/lib/lib__Priv.cmt"
    "_build/install/default/lib/lib/lib__Priv.cmx"
    "_build/install/default/lib/lib/priv.ml"
