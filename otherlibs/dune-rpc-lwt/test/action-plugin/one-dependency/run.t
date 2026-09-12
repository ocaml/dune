This test checks that executable that uses 'dynamic-run'
and requires one dependency can be successfully run.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target some_dependency)
  >  (action (with-stdout-to %{target} (echo "Hello from some_dependency!"))))
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > \
  > (rule
  >  (target held-target)
  >  (action
  >   (dynamic-run ./foo.exe hold "$PWD/connection" "$PWD/release")))
  > EOF

  $ cp ./bin/foo.exe ./

  $ env DUNE_DYNAMIC_RUN_ACTION_ID=1 DUNE_RPC='unix:path=/no-such-dune-rpc-socket' ./foo.exe
  unable to connect to dune rpc server: connect(): No such file or directory
  [1]

  $ dune runtest
  Hello from some_dependency!

A second client can currently initialize the same active action from another
RPC session. This is not necessarily a bug: we may support independent clients
within one action, for example so libraries can discover their own dependencies.
This test covers initialization only, not dependency requests by the second
client.

  $ rm -f connection release
  $ dune build held-target > build.output 2>&1 &
  $ build_pid=$!
  $ for _ in $(seq 1 100); do
  >   test -f connection && break
  >   sleep 0.05
  > done
  $ test -f connection
  $ action_id=$(sed -n 1p connection)
  $ dune_rpc=$(sed -n 2p connection)
  $ (cd _build/default &&
  >  env DUNE_DYNAMIC_RUN_ACTION_ID="$action_id" DUNE_RPC="$dune_rpc" \
  >    ./foo.exe initialize)
  $ touch release
  $ wait "$build_pid"
  $ cat _build/default/held-target
  held
