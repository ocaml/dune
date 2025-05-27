Demonstrate running "dune runtest" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project

Define a test that just prints "Hello, World!"
  $ cat > dune << 'EOF'
  > (rule
  >  (alias runtest)
  >  (action (echo "Hello, World!")))
  > EOF

Build the project once before starting the watch server so the watch server
starts immediately.
  $ dune build
  $ dune build --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Hello, World!File "dune", lines 1-3, characters 0-45:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (run false)))
  Command exited with code 1.
  Had 1 error, waiting for filesystem changes...

Test that we can run a test while another instance of dune is running in watch
mode:
  $ dune runtest
  Success

Test that passing extra arguments to `dune runtest` prints a warning when
running concurrently with another instance of dune in watch mode:
  $ dune runtest --auto-promote 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: Your build request is being forwarded to a running Dune instance
  (pid: PID) so most command-line arguments will be ignored.
  Success

Define and run a test that fails:
  $ cat > dune << 'EOF'
  > (rule
  >  (alias runtest)
  >  (action (run false)))
  > EOF

  $ dune runtest
  File "dune", lines 1-3, characters 0-45:
  1 | (rule
  2 |  (alias runtest)
  3 |  (action (run false)))
  Command exited with code 1.
  Error: Build failed with 1 error.
  [1]

  $ dune shutdown
  $ wait
