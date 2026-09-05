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

An unknown action ID is rejected without disrupting the active action. The
subsequent request with the real ID must still initialize successfully.

  $ (cd _build/default &&
  >  env DUNE_DYNAMIC_RUN_ACTION_ID=missing-action DUNE_RPC="$dune_rpc" \
  >    ./foo.exe initialize)
  dune rpc error: unknown dynamic action "missing-action"
  [1]
  $ (cd _build/default &&
  >  env DUNE_DYNAMIC_RUN_ACTION_ID="$action_id" DUNE_RPC="$dune_rpc" \
  >    ./foo.exe initialize)
  $ touch release
  $ wait "$build_pid"
  $ cat _build/default/held-target
  held

A plugin lends its job slot while waiting for dependencies, including inputs
that require subprocesses rather than built-in Dune actions.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target scheduled-input)
  >  (action (run sh -c "printf scheduled > scheduled-input")))
  > (rule
  >  (alias scheduled)
  >  (action (dynamic-run ./foo.exe read scheduled-input)))
  > (rule
  >  (target parallel-first)
  >  (action (run sh -c "printf first > parallel-first")))
  > (rule
  >  (target parallel-second)
  >  (action (run sh -c "printf second > parallel-second")))
  > (rule
  >  (alias parallel)
  >  (action
  >   (dynamic-run ./foo.exe parallel parallel-first parallel-second)))
  > (rule
  >  (target nested-leaf)
  >  (action (run sh -c "printf leaf > nested-leaf")))
  > (rule
  >  (target nested-input)
  >  (action
  >   (progn
  >    (dynamic-run ./foo.exe read nested-leaf)
  >    (write-file nested-input nested))))
  > (rule
  >  (alias nested)
  >  (action (dynamic-run ./foo.exe read nested-input)))
  > EOF

Ignore signal-handler output when terminating a deadlocked build, but preserve
all diagnostics when the build exits without timing out.

  $ build_with_timeout() {
  >   output=$(mktemp)
  >   status=0
  >   timeout 3 dune build "$@" >"$output" 2>&1 || status=$?
  >   if [ "$status" -ne 124 ]; then cat "$output"; fi
  >   rm "$output"
  >   return "$status"
  > }

  $ build_with_timeout -j 1 @scheduled
  scheduled

Overlapping requests must lend only one slot, not one slot per request.

  $ build_with_timeout -j 1 @parallel
  first,second

Nested dynamic actions must be able to lend their slots independently.

  $ build_with_timeout -j 1 @nested
  leaf
  nested

Weighted dynamic actions lend their full reservation, including when nested.
Reservations larger than the concurrency limit are capped before lending.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using action-plugin 0.1)
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target weighted-leaf)
  >  (action (run sh -c "printf leaf > weighted-leaf")))
  > (rule
  >  (target weighted-input)
  >  (job_slots (run echo 2))
  >  (action
  >   (progn
  >    (dynamic-run ./foo.exe read weighted-leaf)
  >    (write-file weighted-input weighted))))
  > (rule
  >  (alias weighted)
  >  (job_slots (run echo 2))
  >  (action (dynamic-run ./foo.exe read weighted-input)))
  > EOF
  $ timeout 5 dune build -j 2 @weighted
  leaf
  weighted
  $ timeout 5 dune build -j 1 --build-dir=_build-weighted @weighted
  leaf
  weighted
