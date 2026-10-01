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

Both workspace providers contribute to the recipe's dependency layout.

  $ dune build out
  building consumer
  $ cat _build/default/out
  ok
