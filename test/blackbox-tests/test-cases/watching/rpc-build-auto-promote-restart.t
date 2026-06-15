RPC build requests wait for rebuilds caused by auto-promotion before completing.

The first build of the alias below fails the diff action and auto-promotes the
correction. The source change invalidates the just-finished build, so the RPC
request must not complete with the initial failure; it should wait for the
watch loop to rebuild the alias successfully.

  $ make_dune_project 3.23
  $ cat > source <<EOF
  > old
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (target source.corrected)
  >  (action (write-file %{target} "new\n")))
  > (rule
  >  (alias repro)
  >  (action (diff source source.corrected)))
  > EOF

  $ start_dune --auto-promote
  $ with_timeout dune rpc build --wait '(alias repro)'
  Success
  $ cat source
  new
  $ stop_dune_quiet

Unrelated RPC requests may complete before a later auto-promotion restart caused
by another request in the same batch.

  $ wait_for_line_with_timeout () {
  >   output="$1"
  >   line="$2"
  >   iterations="$3"
  >   while ! grep -qx "$line" "$output" 2>/dev/null
  >   do
  >     if [ "$iterations" = 0 ]; then return 124; fi
  >     iterations=$((iterations - 1))
  >     sleep 0.01
  >   done
  > }
  $ cat > source2 <<EOF
  > old
  > EOF
  $ fast_started="$(mktemp -u)"
  $ slow_started="$(mktemp -u)"
  $ fast_release="$(mktemp -u)"
  $ slow_release="$(mktemp -u)"
  $ mkfifo "$fast_release"
  $ mkfifo "$slow_release"
  $ cat > dune <<EOF
  > (rule
  >  (target fast-auto)
  >  (action
  >   (bash "touch '$fast_started'; read _ < '$fast_release'; echo fast > fast-auto")))
  > (rule
  >  (target source2.corrected)
  >  (action
  >   (bash "touch '$slow_started'; read _ < '$slow_release'; printf 'new\n' > source2.corrected")))
  > (rule
  >  (alias repro-auto)
  >  (action (diff source2 source2.corrected)))
  > EOF

  $ start_dune -j 2 --auto-promote
  $ dune rpc build --wait fast-auto > fast-auto.out 2>&1 &
  $ FAST=$!
  $ dune rpc build --wait '(alias repro-auto)' > auto-promote.out 2>&1 &
  $ AUTO_PROMOTE=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$fast_started"
  $ with_timeout dune_cmd wait-for-file-to-appear "$slow_started"
  $ printf '.\n' > "$fast_release"
  $ if wait_for_line_with_timeout fast-auto.out Success 200; then
  >   echo "fast rpc request finished"
  > else
  >   echo "fast rpc request timed out"
  > fi
  fast rpc request timed out
  $ cat source2
  old

  $ printf '.\n' > "$slow_release"
  $ wait_for_pid_to_exit_with_timeout "$FAST" 200 || (cat fast-auto.out; false)
  $ wait "$FAST"
  $ wait_for_pid_to_exit_with_timeout "$AUTO_PROMOTE" 200 || (cat auto-promote.out; false)
  $ wait "$AUTO_PROMOTE"
  $ cat auto-promote.out
  Success
  $ cat source2
  new
  $ stop_dune_quiet
