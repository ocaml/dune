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
  Success, waiting for filesystem changes...

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

Demonstrate printing a warning if arguments are passed that would be ignored
due to how Dune builds via RPC:
  $ dune exec --force ./foo.exe 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: Your build request is being forwarded to a running Dune instance
  (pid: PID). Note that certain command line arguments may be ignored.
  foo

Demonstrate trying to run exec in watch mode while another watch server is running:
  $ dune exec ./foo.exe --watch 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Error: Another instance of dune (pid: PID) has locked the _build
  directory. Refusing to start a new watch server until no other instances of
  dune are running.

Demonstrate running an executable via an absolute path:
  $ dune exec $(which echo) "baz"
  baz

  $ dune shutdown
  $ wait
