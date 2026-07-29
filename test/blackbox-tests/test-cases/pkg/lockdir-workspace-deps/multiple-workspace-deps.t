A lock-dir package depends on multiple workspace packages.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-lib-a))
  > (package (name ws-lib-b))
  > EOF

  $ mkdir src-a src-b
  $ cat > src-a/dune <<EOF
  > (library
  >  (name ws_lib_a)
  >  (public_name ws-lib-a))
  > EOF
  $ cat > src-a/ws_lib_a.ml <<EOF
  > let value_a = "from lib a"
  > EOF

  $ cat > src-b/dune <<EOF
  > (library
  >  (name ws_lib_b)
  >  (public_name ws-lib-b))
  > EOF
  $ cat > src-b/ws_lib_b.ml <<EOF
  > let value_b = "from lib b"
  > EOF

The lock dir contains one package "consumer" that declares both
workspace libraries as dependencies:

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends ws-lib-a ws-lib-b)
  > (build (run echo "building consumer"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The build succeeds. Both workspace libraries appear in the same layout
digest under .packages:

  $ dune build out
  building consumer

  $ find _build/install/default/.packages -type f -o -type l | censor | sort
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/META
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/dune-package
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.a
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cma
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cmi
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cmt
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cmx
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cmxa
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.cmxs
  _build/install/default/.packages/$DIGEST/lib/ws-lib-a/ws_lib_a.ml
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/META
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/dune-package
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.a
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cma
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cmi
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cmt
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cmx
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cmxa
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.cmxs
  _build/install/default/.packages/$DIGEST/lib/ws-lib-b/ws_lib_b.ml
