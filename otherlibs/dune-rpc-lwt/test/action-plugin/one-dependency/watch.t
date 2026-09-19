Cancellation restarts the action, but subsequent changes are not observed:
RPC requests do not record Memo dependency edges back to the caller.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cp bin/foo.exe .
  $ printf wait > watch-control
  $ cat > slow.sh <<'EOF'
  > printf started > watched-started
  > if grep -q wait watch-control; then
  >   while :; do sleep 1; done
  > fi
  > cat watch-control > watched-input
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target watched-input)
  >  (deps watch-control slow.sh)
  >  (action (run sh slow.sh)))
  > (rule
  >  (target watched-output)
  >  (action (with-stdout-to watched-output
  >   (dynamic-run ./foo.exe read watched-input))))
  > EOF
  $ wait_for_value () {
  >   for i in $(seq 1 1000); do
  >     if [ "$(cat "$1" 2>/dev/null)" = "$2" ]; then return; fi
  >     sleep 0.01
  >   done
  >   cat watch.log
  >   return 1
  > }
  $ wait_for_successes () {
  >   for i in $(seq 1 1000); do
  >     if [ "$(grep -c 'Success, waiting' watch.log)" -ge "$1" ]; then return; fi
  >     sleep 0.01
  >   done
  >   cat watch.log
  >   return 1
  > }
  $ mkdir .runtime
  $ export XDG_RUNTIME_DIR="$PWD/.runtime"
  $ timeout -k 1 30 dune build -w -j 2 watched-output > watch.log 2>&1 &
  $ watch_pid=$!
  $ trap 'kill -TERM "$watch_pid" 2>/dev/null; wait "$watch_pid" 2>/dev/null' EXIT
  $ wait_for_value _build/default/watched-started started
  $ printf ready > watch-control
  $ wait_for_value _build/default/watched-output ready
  $ wait_for_successes 1
  $ cat _build/default/watched-output
  ready
  $ printf again > watch-control
  $ dune rpc flush-file-watcher
  $ wait_for_successes 2
  $ cat _build/default/watched-output
  ready
  $ kill -TERM "$watch_pid"
  $ wait "$watch_pid"
  [130]
  $ trap - EXIT

A fresh build reads the updated input.

  $ dune build --build-dir _build-fresh watched-output
  $ cat _build-fresh/default/watched-output
  again
