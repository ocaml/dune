Demonstrate running "dune build" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project
  $ echo '(executable (name foo))' > dune
  $ echo 'let () = print_endline "Hello, World!"' > foo.ml

Watch a separate target so the client request, not the eager build, builds
foo.exe. Keep background diagnostics out of the client transcript.

  $ dune build dune-project --watch > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server

Demonstrate that we can run "dune build" while the watch server is running.
  $ with_timeout dune build
  Success
  $ _build/default/foo.exe
  Hello, World!

Demonstrate that error messages are still printed by "dune build" when it's
acting as an RPC client while running concurrently with an RPC server.
  $ echo 'let () = unknown_identifier ()' > foo.ml
  $ with_timeout dune rpc flush-file-watcher --wait
  $ with_timeout dune build
  File "$TESTCASE_ROOT/foo.ml", line 1, characters 9-27:
  1 | let () = unknown_identifier ()
               ^^^^^^^^^^^^^^^^^^
  Unbound value unknown_identifier
  Error: Build failed with 1 error.
  [1]

  $ stop_dune_quiet
