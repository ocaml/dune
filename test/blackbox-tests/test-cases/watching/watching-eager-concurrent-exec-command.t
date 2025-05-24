Demonstrate running "dune exec" concurrently with an eager rpc server.

  $ echo '(lang dune 3.18)' > dune-project
  $ echo '(executable (name foo))' > dune
  $ echo 'let () = print_endline "Hello, World!"' > foo.ml
  $ touch README.md

Just watch the readme file so we don't accidentally build foo.exe before
testing the --no-build option:
  $ dune build README.md --watch &
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Demonstrate handling the --no-build option:
  $ dune exec --no-build ./foo.exe 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: A running dune (pid: PID) instance has locked the build
  directory. If this is not the case, please delete "_build/.lock".
  While one instance of Dune is already running, subsequent invocations of Dune
  will run with reduced functionality and some command-line arguments will be
  ignored.
  Error: Program './foo.exe' isn't built yet. You need to build it first or
  remove the '--no-build' option.

Demonstrate running an executable from the current project:
  $ dune exec ./foo.exe 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: A running dune (pid: PID) instance has locked the build
  directory. If this is not the case, please delete "_build/.lock".
  While one instance of Dune is already running, subsequent invocations of Dune
  will run with reduced functionality and some command-line arguments will be
  ignored.
  Hello, World!

Demonstrate running an executable from PATH:
  $ dune exec echo "Hello, World!" 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: A running dune (pid: PID) instance has locked the build
  directory. If this is not the case, please delete "_build/.lock".
  While one instance of Dune is already running, subsequent invocations of Dune
  will run with reduced functionality and some command-line arguments will be
  ignored.
  Warning: As this is not the main instance of Dune it is unable to locate the
  executable "echo" within this project. Dune will attempt to resolve the
  executable's name within your PATH only.
  Hello, World!

Demonstrate running an executable via an absolute path:
  $ dune exec $(which echo) "Hello, World!" 2>&1 | sed 's/pid: [0-9]*/pid: PID/g'
  Warning: A running dune (pid: PID) instance has locked the build
  directory. If this is not the case, please delete "_build/.lock".
  While one instance of Dune is already running, subsequent invocations of Dune
  will run with reduced functionality and some command-line arguments will be
  ignored.
  Hello, World!

  $ dune shutdown
  $ wait
