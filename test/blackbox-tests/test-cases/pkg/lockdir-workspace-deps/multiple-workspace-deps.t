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

Lock-dir validation does not currently recognise workspace packages as
valid dependency targets. Both missing workspace deps are reported:

  $ dune build out 2>&1
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-17:
  The package "consumer" depends on the package "ws-lib-a", but "ws-lib-a" does
  not appear in the lockdir _build/_private/default/.lock/dune.lock.
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 18-26:
  The package "consumer" depends on the package "ws-lib-b", but "ws-lib-b" does
  not appear in the lockdir _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]
