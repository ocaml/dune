The action remains silent and blocked while the status output is sampled.
Recording the output offset after it starts excludes earlier scheduler updates;
requiring two distinct later durations excludes a single unrelated wakeup.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ STARTED="$PWD/started"
  $ RELEASE="$PWD/release"
  $ cat > dune <<EOF
  > (rule
  >  (target slow-target)
  >  (action
  >   (progn
  >    (system "touch '$STARTED'; while test ! -f '$RELEASE'; do sleep 0.1; done")
  >    (write-file %{target} done))))
  > EOF

Force the progress display even though stderr is redirected, then wait until
the action is blocked.

  $ INSIDE_EMACS=1 DUNE_CONFIG__THREADED_CONSOLE=disabled \
  >   dune build --display progress slow-target > build-output 2>&1 &
  $ BUILD_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$STARTED"
  $ OUTPUT_SIZE=$(wc -c < build-output)
  $ sleep 1
  $ tail -c +$((OUTPUT_SIZE + 1)) build-output \
  > | (grep -a -E -o "\[[0-9]+\.[0-9]s\]" || true) \
  > | sort -u \
  > | awk 'END { print NR >= 2 ? "build duration refreshed repeatedly" : "build duration did not refresh repeatedly" }'
  build duration did not refresh repeatedly

  $ touch "$RELEASE"
  $ wait_for_pid_to_exit_with_timeout "$BUILD_PID" 200 || (cat build-output; false)
  $ wait "$BUILD_PID"
