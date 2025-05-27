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
  Hello, World!

Test that we can run a test while another instance of dune is running in watch
mode:
  $ dune runtest 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Success

Test that passing extra arguments to `dune runtest` prints a warning when
running concurrently with another instance of dune in watch mode:
  $ dune runtest --auto-promote 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: Your build request is being forwarded to a running Dune instance
  (pid: PID) so most command-line arguments will be ignored.
  Success

  $ dune shutdown
  $ wait
