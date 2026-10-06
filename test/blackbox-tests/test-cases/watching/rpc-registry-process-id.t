The RPC registry uses the server's OS process ID, not a process-local handle,
in both the filename and the record. Shutdown removes the server's entry.

  $ setup_xdg_runtime_dir
  $ os_type=$(ocamlc -config-var os_type)
  $ if [ "$os_type" = Win32 ]; then
  >   export XDG_RUNTIME_DIR="$(cygpath -m "$XDG_RUNTIME_DIR")"
  > fi
  $ echo '(lang dune 3.24)' > dune-project
  $ cleanup () {
  >   "$timeout" 2 dune shutdown >/dev/null 2>&1 || :
  > }
  $ trap cleanup EXIT

Start the executable directly so that the shell PID identifies the server,
not the wrapper used by start_dune. Cygwin reports the native PID as WINPID.

  $ dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server
  $ NATIVE_PID=$DUNE_PID
  $ if [ "$os_type" = Win32 ]; then
  >   NATIVE_PID=$(ps -l -p "$DUNE_PID" | awk '
  >     NR == 1 { for (i = 1; i <= NF; i++) if ($i == "WINPID") col = i }
  >     NR == 2 && col { print $col }')
  > fi
  $ test -n "$NATIVE_PID"
  $ find "$XDG_RUNTIME_DIR/dune/rpc" -type f | wc -l | tr -d ' '
  1
  $ registry_file="$XDG_RUNTIME_DIR/dune/rpc/$NATIVE_PID.csexp"
  $ test -f "$registry_file"
  $ grep -Fq "(3:pid${#NATIVE_PID}:$NATIVE_PID)" "$registry_file"
  $ stop_dune_quiet
  $ if [ "$os_type" = Win32 ]; then wait "$DUNE_PID"; fi
  $ find "$XDG_RUNTIME_DIR/dune/rpc" -type f | wc -l | tr -d ' '
  0
