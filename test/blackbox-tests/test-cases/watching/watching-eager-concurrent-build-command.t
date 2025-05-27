Demonstrate running "dune build" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project
  $ echo '(executable (name foo))' > dune
  $ echo 'let () = print_endline "Hello, World!"' > foo.ml

Build the project once before starting the watch server so the watch server starts immediately.
  $ dune build
  $ dune build --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  File "foo.ml", line 1, characters 9-21:
  1 | let () = print_endlin "Hello, World!"
               ^^^^^^^^^^^^
  Error: Unbound value print_endlin
  Hint: Did you mean print_endline?
  Had 1 error, waiting for filesystem changes...
  File "foo.ml", line 1, characters 9-21:
  1 | let () = print_endlin "Hello, World!"
               ^^^^^^^^^^^^
  Error: Unbound value print_endlin
  Hint: Did you mean print_endline?
  Had 1 error, waiting for filesystem changes...

Demonstrate that we can run "dune build" while the watch server is running.
  $ dune build
  Success

Demonstrate that a warning is displayed when extra arguments are passed to
"dune build", since those arguments will be ignored.
  $ dune build --auto-promote 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: Your build request is being forwarded to a running Dune instance
  (pid: PID) so most command-line arguments will be ignored.
  Success

Demonstrate that error messages are still printed by "dune build" when it's
acting as an RPC client while running concurrently with an RPC server.
  $ echo 'let () = print_endlin "Hello, World!"' > foo.ml
  $ dune build
  File "$TESTCASE_ROOT/foo.ml", line 1, characters 9-21:
  1 | let () = print_endlin "Hello, World!"
               ^^^^^^^^^^^^
  Unbound value print_endlin
  Hint: Did you mean print_endline?
  Error: Build failed with 1 error.
  [1]

  $ dune shutdown
  $ wait
