Compare the default RPC endpoints published by concurrent Windows workspaces
and check workspace routing when the endpoints are independent.

  $ setup_xdg_runtime_dir
  $ export XDG_RUNTIME_DIR="$(cygpath -m "$XDG_RUNTIME_DIR")"
  $ unset DUNE_RPC
  $ mkdir a b
  $ for project in a b; do
  >   echo '(lang dune 3.24)' > "$project/dune-project"
  >   cat > "$project/dune" <<EOF
  > (rule
  >  (target x)
  >  (action (with-stdout-to x (echo $project))))
  > EOF
  > done

Do not use RPC shutdown: the unfixed servers share an endpoint with each other
and possibly the test runner. Terminate only the captured fixture processes.

  $ A_PID= B_PID= A_READY= B_READY= endpoint_a= endpoint_b=
  $ cleanup () {
  >   local cleanup_status=0
  >   for fixture_pid in $A_PID $B_PID; do
  >     /bin/kill -f "$fixture_pid" >/dev/null 2>&1 || :
  >     if wait_for_pid_to_exit_with_timeout "$fixture_pid" 200; then
  >       wait "$fixture_pid" 2>/dev/null || :
  >       case "$fixture_pid" in
  >         "$A_PID") A_PID= ;; "$B_PID") B_PID= ;;
  >       esac
  >     else
  >       cleanup_status=$?
  >     fi
  >   done
  >   return "$cleanup_status"
  > }
  $ trap cleanup EXIT

Check complete publication for each root in turn. The independent Windows
registry PID bug can cause the two servers to reuse a registry filename.

  $ wait_for_registry () {
  >   "$timeout" 2 sh -c '
  >     until (
  >       for entry in "$XDG_RUNTIME_DIR"/dune/rpc/*; do
  >         grep -Fq -- "$1)" "$entry" 2>/dev/null &&
  >         dune internal sexp-pp --format=csexp "$entry" \
  >           >/dev/null 2>&1 && exit 0
  >       done
  >       exit 1
  >     ); do sleep 0.01; done' sh "$1"
  > }
  $ cd a
  $ native_root=$(cygpath -m "$PWD")
  $ dune build --root="$native_root" --passive-watch-mode \
  >   > .#dune-output 2>&1 &
  $ A_PID=$!
  $ wait_for_registry "$native_root" &&
  >   endpoint_a=$(cat _build/.rpc/dune) &&
  >   grep -Eq '^tcp:host=127\.0\.0\.1,port=[1-9][0-9]*$' _build/.rpc/dune &&
  >   grep -Fq -- "$endpoint_a)" "$XDG_RUNTIME_DIR"/dune/rpc/* && A_READY=1
  $ cd ../b
  $ native_root=$(cygpath -m "$PWD")
  $ dune build --root="$native_root" --passive-watch-mode \
  >   > .#dune-output 2>&1 &
  $ B_PID=$!
  $ wait_for_registry "$native_root" &&
  >   endpoint_b=$(cat _build/.rpc/dune) &&
  >   grep -Eq '^tcp:host=127\.0\.0\.1,port=[1-9][0-9]*$' _build/.rpc/dune &&
  >   grep -Fq -- "$endpoint_b)" "$XDG_RUNTIME_DIR"/dune/rpc/* && B_READY=1
  $ cd ..
  $ kill -0 "$A_PID" "$B_PID"
  $ test "$endpoint_a" != "$endpoint_b"

Shared endpoints cannot safely address either fixture: on main, make no RPC
calls. Once independent, verify the actual outputs in both workspaces.

  $ if [ "$A_READY$B_READY" = 11 ] &&
  >    [ "$endpoint_a" != "$endpoint_b" ] &&
  >    [ "$endpoint_a" != tcp:host=127.0.0.1,port=8587 ] &&
  >    [ "$endpoint_b" != tcp:host=127.0.0.1,port=8587 ]; then
  >   (cd a && build_quiet x && cat _build/default/x)
  >   (cd b && build_quiet x && cat _build/default/x)
  > else
  >   echo 'workspaces share an RPC endpoint'
  > fi
  ab
  $ cleanup
