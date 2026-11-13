Dune terminates a plugin process that has not connected when the build is
interrupted.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.26)
  > (using plugin 0.1)
  > EOF
  $ mkdir plugin use
  $ cat > plugin/dune <<'EOF'
  > (executable
  >  (name rules)
  >  (libraries unix))
  > (plugin
  >  (name generate)
  >  (executable ./rules.exe))
  > EOF
  $ cat > plugin/rules.ml <<'EOF'
  > let () =
  >   let output = open_out "started" in
  >   Printf.fprintf output "%d\n%!" (Unix.getpid ());
  >   close_out output;
  >   while true do
  >     Unix.sleep 1
  >   done
  > EOF
  $ cat > use/dune <<'EOF'
  > (plugin.generate)
  > EOF

  $ dune build use >dune.output 2>&1 &
  $ DUNE_PID=$!
  $ with_timeout dune_cmd wait-for-file-to-appear plugin/started
  $ PLUGIN_PID=$(cat plugin/started)
  $ kill -INT "$DUNE_PID"
  $ wait_for_pid_to_exit_with_timeout "$DUNE_PID" 200 || \
  >   (kill -KILL "$DUNE_PID" "$PLUGIN_PID" 2>/dev/null; false)
  $ wait "$DUNE_PID"
  [130]
  $ if kill -0 "$PLUGIN_PID" 2>/dev/null; then
  >   echo "plugin process is still running"
  >   kill -KILL "$PLUGIN_PID"
  > fi
