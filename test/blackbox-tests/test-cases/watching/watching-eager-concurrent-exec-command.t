Demonstrate running "dune exec" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project
  $ echo '(executable (name foo))' > dune
  $ echo 'let () = print_endline "foo"' > foo.ml
  $ touch README.md

Just watch the readme file so we don't accidentally build foo.exe before
testing the --no-build option:
  $ dune build README.md --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Make sure the RPC server is properly started:
  $ dune rpc ping --wait
  Server appears to be responding normally

Demonstrate handling the --no-build option:
  $ dune exec --no-build ./foo.exe
  Error: Program './foo.exe' isn't built yet. You need to build it first or
  remove the '--no-build' option.
  [1]

Demonstrate running an executable from the current project:
  $ dune exec ./foo.exe
  foo

Demonstrate running an executable from PATH:
  $ dune exec echo "bar"
  Warning: As this is not the main instance of Dune it is unable to locate the
  executable "echo" within this project. Dune will attempt to resolve the
  executable's name within your PATH only.
  bar

Demonstrate trying to run exec in watch mode while another watch server is running:
  $ dune exec ./foo.exe --watch 2>&1 | tr '\n' ' ' | sed 's/(pid: [0-9]*)/(pid: PID)/'
  Error: Another instance of dune (pid: PID) has locked the _build directory. Refusing to start a new watch server until no other instances of dune are running. 
  [1]

Demonstrate running an executable via an absolute path:
  $ dune exec $(which echo) "baz"
  baz

  $ dune shutdown
  $ wait

Demonstrate starting "dune exec --watch" when no other watch server is running.
The watch server started by exec also accepts build requests forwarded from
ordinary dune commands.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target rpc-target)
  >  (action (with-stdout-to rpc-target (echo rpc))))
  > (rule
  >  (target runtest-output)
  >  (action (with-stdout-to runtest-output (echo runtest))))
  > (alias
  >  (name runtest)
  >  (deps runtest-output))
  > EOF
  $ ((dune exec ./foo.exe --watch > .#exec-watch-output 2>&1) || (echo exit $? >> .#exec-watch-output)) &
  $ EXEC_DUNE_PID=$!
  $ wait_for_rpc_server
  $ dune build rpc-target
  Success
  $ cat _build/default/rpc-target
  rpc
  $ dune runtest
  Success
  $ cat _build/default/runtest-output
  runtest
  $ dune shutdown
  $ wait_for_pid_to_exit_with_timeout "$EXEC_DUNE_PID" 200 || (cat .#exec-watch-output; false)
  $ wait "$EXEC_DUNE_PID"
  $ grep -m1 '^foo$' .#exec-watch-output
  foo
