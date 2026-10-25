An inotify overflow must release a sync whose notification was lost (#16625).

Replace the first inotify event's watch, mask and cookie with an IN_Q_OVERFLOW
record, preserving its name length. Suppress the close of the first sync file
so its IN_CLOSE_WRITE notification cannot rescue the lost IN_CREATE event.
The filters restrict injection to that file and the inotify descriptor.

  $ setup_xdg_runtime_dir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target x) (action (write-file x built)))
  > EOF

Pre-create the client's output file so its creation cannot be the first
inotify event, even though Dune excludes it from source invalidations.

  $ touch .#client-output

The inotify event header uses native byte order.

  $ case "$(printf '\001\000\000\000' | od -An -tu4)" in
  >   *16777216) overflow=ffffffff0000400000000000 ;;
  >   *) overflow=ffffffff0040000000000000 ;;
  > esac
  $ strace -f -qq -o .#strace \
  >   -P anon_inode:inotify -P "$PWD/_build/.sync/0" \
  >   -e trace=read,close \
  >   -e inject=read:poke_exit=@arg2=$overflow:when=1 \
  >   -e inject=close:retval=0 \
  >   dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ STRACE_PID=$!
  $ with_timeout dune rpc ping --wait > /dev/null 2>&1

The rule finishes, but the client currently hangs waiting for the lost sync.

  $ timeout 5 dune build x > .#client-output 2>&1; echo "client exit: $?"
  client exit: 124
  $ cat _build/default/x; echo
  built
  $ grep -c '(INJECTED' .#strace
  2
  $ with_timeout dune shutdown
  $ wait "$STRACE_PID"

The overflow reached Dune, but the build could not finish and restart.

  $ dune trace cat | jq -s '
  > {overflows: [.[] | select(.name == "queue_overflow")] | length,
  >  builds: [.[] | select(.name == "build-start")] | length}'
  {
    "overflows": 1,
    "builds": 1
  }
