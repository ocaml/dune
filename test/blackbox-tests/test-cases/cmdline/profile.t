# Bug #4632

  $ dune build -p
  dune: option '-p' needs an argument
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build --root . --verbose 2>&1 | grep "; profile"
   ; profile = Dev

  $ dune build -p foo --verbose 2>&1 | grep "; profile"
   ; profile = Release

  $ DUNE_PROFILE="bar" dune build --root . --verbose 2>&1 | grep "; profile"
   ; profile = User_defined "bar"

  $ DUNE_PROFILE="bar" dune build -p foo --verbose 2>&1 | grep "; profile"
   ; profile = Release
