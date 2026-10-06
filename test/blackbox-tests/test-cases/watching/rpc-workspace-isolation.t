Concurrent workspaces have independent RPC endpoints and build requests.
The registry publishes the bound address. Stopping one server must not stop
the other.

  $ setup_xdg_runtime_dir
  $ case "$(uname -s)" in
  >   CYGWIN*|MINGW*) export XDG_RUNTIME_DIR="$(cygpath -m "$XDG_RUNTIME_DIR")" ;;
  > esac
  $ os_type=$(ocamlc -config-var os_type)
  $ mkdir a b
  $ for project in a b; do
  >   echo '(lang dune 3.24)' > "$project/dune-project"
  >   cat > "$project/dune" <<EOF
  > (rule
  >  (target x)
  >  (action (with-stdout-to x (echo $project))))
  > EOF
  > done

The cleanup retries both markers so that the regression case also cleans up
when Windows has published the same endpoint for two servers.

  $ root="$PWD"
  $ cleanup () {
  >   for attempt in 1 2; do
  >     for project in a b; do
  >       (cd "$root/$project" && "$timeout" 2 dune shutdown) \
  >         >/dev/null 2>&1 || :
  >     done
  >   done
  > }
  $ trap cleanup EXIT
  $ cd a
  $ dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ A_PID=$!
  $ wait_for_rpc_server

Before starting the second server, check that the registry contains the
published address rather than the requested port zero or the old default.

  $ if [ -f _build/.rpc/dune ]; then
  >   grep -Fq -- "$(cat _build/.rpc/dune)" "$XDG_RUNTIME_DIR"/dune/rpc/*
  > fi
  $ cd ../b
  $ dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ B_PID=$!
  $ wait_for_rpc_server
  $ cd ..

On Windows the marker files must contain different TCP addresses. On Unix
the markers are independent domain sockets instead of regular files.

  $ if [ -f a/_build/.rpc/dune ]; then
  >   test "$(cat a/_build/.rpc/dune)" != "$(cat b/_build/.rpc/dune)" || {
  >     echo 'shared RPC endpoint'
  >     false
  >   }
  > fi

Both RPC builds must execute in their own workspace, not merely receive a
successful ping from whichever server owns the shared port.

  $ cd a
  $ build_quiet x
  $ cat _build/default/x
  a
  $ cd ../b
  $ build_quiet x
  $ cat _build/default/x
  b

Native Windows shutdown must also exit cleanly, not only release the marker.
The Unix wait helper avoids a known Linux shell PID-aliasing issue.

  $ cd ../a
  $ DUNE_PID="$A_PID" stop_dune_quiet
  $ if [ "$os_type" = Win32 ]; then wait "$A_PID"; fi
  $ cd ../b
  $ with_timeout_quiet dune rpc ping
  $ DUNE_PID="$B_PID" stop_dune_quiet
  $ if [ "$os_type" = Win32 ]; then wait "$B_PID"; fi
