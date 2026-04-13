Promoting a binary that is currently running should succeed. The promotion
uses atomic rename, which replaces the directory entry while the running
process keeps its file descriptor.

Regression test for https://github.com/ocaml/dune/issues/3484

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name server)
  >  (libraries unix)
  >  (promote (until-clean) (into .)))
  > EOF

The server writes a ready file on startup and exits after 30 seconds to
avoid leaking a process if the test fails.

  $ cat > server.ml <<'EOF'
  > let () =
  >   let ready = Filename.concat (Sys.getcwd ()) "ready" in
  >   let oc = open_out ready in
  >   close_out oc;
  >   print_endline "server v1";
  >   Unix.sleepf 30.0
  > EOF

  $ dune build ./server.exe

Run the promoted binary directly (not via dune exec) because the test
is specifically about promoting over a running binary on disk.

  $ ./server.exe &
  server v1
  $ PID=$!

  $ dune_cmd wait-for-file-to-appear ready
  $ rm -f ready

Modify the source and rebuild while the promoted binary is running.

  $ cat > server.ml <<'EOF'
  > let () =
  >   let ready = Filename.concat (Sys.getcwd ()) "ready" in
  >   let oc = open_out ready in
  >   close_out oc;
  >   print_endline "server v2";
  >   Unix.sleepf 30.0
  > EOF

  $ dune build ./server.exe

The promoted binary should be updated. Kill the old server.

  $ kill $PID 2>/dev/null; wait $PID 2>/dev/null || true

Verify the new binary works.

  $ ./server.exe &
  server v2
  $ NEW_PID=$!
  $ dune_cmd wait-for-file-to-appear ready
  $ kill $NEW_PID 2>/dev/null; wait $NEW_PID 2>/dev/null || true
