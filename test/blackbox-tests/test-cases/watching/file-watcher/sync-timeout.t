Retry a lost sync notification even when the watcher reports no overflow.

Drop the first two sync-file create notifications by clearing their event
masks. Suppress their close notifications too. A timeout must issue a fresh
marker for the same waiter, and must itself be retried if its event is lost.

  $ setup_xdg_runtime_dir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target x) (action (write-file x built)))
  > EOF
  $ strace -f -qq -o .#strace \
  >   -P anon_inode:inotify \
  >   -P "$PWD/_build/.sync/0.0" -P "$PWD/_build/.sync/0.1" \
  >   -e trace=read,close \
  >   -e inject=read:poke_exit=@arg2=ffffffff0000000000000000:when=1..2 \
  >   -e inject=close:retval=0 \
  >   dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ STRACE_PID=$!
  $ with_timeout dune rpc ping --wait > /dev/null 2>&1

The client must wait for both retries and complete when a fresh marker is
observed, rather than hang on either lost notification.

  $ timeout 20 dune build x > _build/client-output 2>&1; echo "client exit: $?"
  client exit: 0
  $ cat _build/client-output
  Success
  $ test ! -e _build/.sync/0.0
  $ test ! -e _build/.sync/0.1
  $ cat _build/default/x; echo
  built
  $ grep -c '(INJECTED' .#strace
  4

Successful synchronization cancels its timer, so later healthy builds do not
wait for the five-second deadline.

  $ with_timeout dune build x
  Success
  $ with_timeout dune shutdown
  $ wait "$STRACE_PID"

There is no overflow to trigger a retry. With a five-second timeout for each
attempt, request 0 must be acknowledged after two retries, at least ten seconds
after the build started.

  $ dune trace cat | jq -s '
  > ([.[] | select(.name == "build-start")] | first | .ts) as $start
  > | ([.[] | select(.cat == "file_watcher" and .name == "sync")]
  >    | first) as $sync
  > | {overflows: [.[] | select(.name == "queue_overflow")] | length,
  >    first_sync: $sync.args,
  >    waited_ten_seconds:
  >      (if $sync == null then false else $sync.ts - $start >= 10 end)}'
  {
    "overflows": 0,
    "first_sync": {
      "id": 0,
      "retries": 2
    },
    "waited_ten_seconds": true
  }
