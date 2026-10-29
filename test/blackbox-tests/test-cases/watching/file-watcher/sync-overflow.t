An inotify overflow must retry a sync whose notification was lost (#16625).

Replace the first two inotify events' watch, mask and cookie with IN_Q_OVERFLOW
records, preserving their name lengths. Suppress the close of the first two
sync files so their IN_CLOSE_WRITE notifications cannot rescue the lost
IN_CREATE events. This loses both the original marker and its first retry.
The filters restrict injection to those files and the inotify descriptor.

  $ setup_xdg_runtime_dir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target x) (deps input) (action (copy input x)))
  > EOF
  $ printf built > input

Pre-create the client's output file so its creation cannot be the first
inotify event, even though Dune excludes it from source invalidations.

  $ touch .#client-output

The inotify event header uses native byte order.

  $ case "$(printf '\001\000\000\000' | od -An -tu4)" in
  >   *16777216) overflow=ffffffff0000400000000000 ;;
  >   *) overflow=ffffffff0040000000000000 ;;
  > esac
  $ strace -f -qq -o .#strace \
  >   -P anon_inode:inotify \
  >   -P "$PWD/_build/.sync/0" -P "$PWD/_build/.sync/1" \
  >   -e trace=read,close \
  >   -e inject=read:poke_exit=@arg2=$overflow:when=1..2 \
  >   -e inject=close:retval=0 \
  >   dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ STRACE_PID=$!
  $ with_timeout dune rpc ping --wait > /dev/null 2>&1

The overflow must restart the build, not complete the client with the result
from before invalidation. Both lost markers are removed, and their later
notifications must not acknowledge the retried sync.

  $ timeout 5 dune build x > .#client-output 2>&1; echo "client exit: $?"
  client exit: 0
  $ cat .#client-output
  Success
  $ test ! -e _build/.sync/0
  $ test ! -e _build/.sync/1
  $ cat _build/default/x; echo
  built
  $ grep -c '(INJECTED' .#strace
  4

The same watcher continues to track source changes and synchronize later builds.

  $ printf updated > input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ with_timeout dune build x
  Success
  $ cat _build/default/x; echo
  updated
  $ with_timeout dune shutdown
  $ wait "$STRACE_PID"

There are three builds: the interrupted one, its restart, and the later build.
The first acknowledged sync must have a fresh ID, not either lost marker's ID.

  $ dune trace cat | jq -s '
  > {overflows: [.[] | select(.name == "queue_overflow")] | length,
  >  builds: [.[] | select(.name == "build-start")] | length,
  >  first_sync: [.[] | select(.cat == "file_watcher" and .name == "sync")]
  >              | first | .args.id}'
  {
    "overflows": 2,
    "builds": 3,
    "first_sync": 2
  }
