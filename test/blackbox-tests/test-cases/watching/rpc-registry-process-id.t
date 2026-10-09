Compare the RPC registry filename and serialized PID with the server's
native Windows process ID.

  $ setup_xdg_runtime_dir
  $ export XDG_RUNTIME_DIR="$(cygpath -m "$XDG_RUNTIME_DIR")"
  $ unset DUNE_RPC
  $ echo '(lang dune 3.24)' > dune-project

Do not use RPC shutdown: an unfixed server can share the test runner's endpoint.
Terminate only the captured fixture process, with a bounded wait for exit.

  $ DUNE_PID=
  $ cleanup () {
  >   if [ -n "$DUNE_PID" ]; then
  >     /bin/kill -f "$DUNE_PID" >/dev/null 2>&1 || :
  >     wait_for_pid_to_exit_with_timeout "$DUNE_PID" 200 || return $?
  >     wait "$DUNE_PID" 2>/dev/null || :
  >     DUNE_PID=
  >   fi
  > }
  $ trap cleanup EXIT

Start the executable directly so that the shell PID identifies the server,
not the wrapper used by start_dune. Cygwin reports the native PID as WINPID.

  $ dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ "$timeout" 2 sh -c '
  >   until grep -Eq "\(3:pid[0-9]+:[0-9]+\)" \
  >     "$XDG_RUNTIME_DIR"/dune/rpc/* 2>/dev/null
  >   do sleep 0.01; done'
  $ kill -0 "$DUNE_PID"
  $ NATIVE_PID=$(ps -l -p "$DUNE_PID" | awk '
  >   NR == 1 { for (i = 1; i <= NF; i++) if ($i == "WINPID") col = i }
  >   NR == 2 && col { print $col }')
  $ test -n "$NATIVE_PID"
  $ find "$XDG_RUNTIME_DIR/dune/rpc" -type f | wc -l | tr -d ' '
  1
  $ test -f "$XDG_RUNTIME_DIR/dune/rpc/$NATIVE_PID.csexp"
  [1]
  $ grep -Fq "(3:pid${#NATIVE_PID}:$NATIVE_PID)" "$XDG_RUNTIME_DIR"/dune/rpc/*
  [1]
  $ cleanup
