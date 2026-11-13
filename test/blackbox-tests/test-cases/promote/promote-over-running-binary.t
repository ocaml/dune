Promoting a binary that is currently running should succeed. The promotion
uses atomic rename, which replaces the directory entry while the running
process keeps its file descriptor.

Regression test for https://github.com/ocaml/dune/issues/3484

  $ make_dune_project 3.23

  $ cat > dune <<EOF
  > (executable
  >  (name server)
  >  (libraries unix)
  >  (promote (until-clean) (into .)))
  > EOF

The server flushes its version before creating the ready file, then waits
until explicitly stopped. Clean it up even if the test is interrupted.

  $ PID=
  $ stop_server() {
  >   if [ -n "$PID" ]; then
  >     kill "$PID" 2>/dev/null || true
  >     wait "$PID" 2>/dev/null || true
  >     PID=
  >   fi
  > }
  $ trap stop_server EXIT
  $ trap 'exit 1' INT TERM

  $ cat > server.ml <<'EOF'
  > let () =
  >   print_endline "server v1";
  >   let ready = Filename.concat (Sys.getcwd ()) "ready" in
  >   let oc = open_out ready in
  >   close_out oc;
  >   while true do Unix.pause () done
  > EOF

  $ dune build ./server.exe

Run the promoted binary directly (not via dune exec) because the test
is specifically about promoting over a running binary on disk.

  $ ./server.exe & PID=$!
  server v1

  $ dune_cmd wait-for-file-to-appear ready
  $ rm -f ready

Modify the source and rebuild while the promoted binary is running.

  $ cat > server.ml <<'EOF'
  > let () =
  >   print_endline "server v2";
  >   let ready = Filename.concat (Sys.getcwd ()) "ready" in
  >   let oc = open_out ready in
  >   close_out oc;
  >   while true do Unix.pause () done
  > EOF

  $ dune build ./server.exe

The promoted binary should be updated while the original server is still alive.

  $ kill -0 "$PID"
  $ stop_server

Verify the new binary works.

  $ ./server.exe & PID=$!
  server v2
  $ dune_cmd wait-for-file-to-appear ready
  $ stop_server
