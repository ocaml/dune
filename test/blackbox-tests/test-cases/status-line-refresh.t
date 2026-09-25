A silent, blocked action leaves the build idle. Ignore status output from before
it starts, then check whether the elapsed time advances without other events.

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
  $ i=30
  $ while [ "$i" -gt 0 ]; do
  >   COUNT=$(tail -c +$((OUTPUT_SIZE + 1)) build-output \
  >     | (grep -a -E -o "\[[0-9]+\.[0-9]s\]" || true) | sort -u | wc -l)
  >   if [ "$COUNT" -ge 2 ]; then break; fi
  >   i=$((i - 1))
  >   sleep 0.1
  > done
  $ if [ "$COUNT" -ge 2 ]; then echo "periodic updates"; else echo "no periodic updates"; fi
  no periodic updates

  $ touch "$RELEASE"
  $ wait_for_pid_to_exit_with_timeout "$BUILD_PID" 200 || (cat build-output; false)
  $ wait "$BUILD_PID"

