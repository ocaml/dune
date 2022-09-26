  $ dune runtest --display short
          diff alias runtest
          diff alias runtest

Test that incompatible options are properly reported
----------------------------------------------------

  $ dune build --verbose --display quiet
  dune: Cannot use --verbose and --display simultaneously
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build -p toto --root .
  dune: option '--root' cannot be repeated
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build --for-release-of-packages toto --root .
  dune: option '--root' cannot be repeated
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build --no-config --config x
  dune: Cannot use --config and --no-config simultaneously
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build -p toto --release
  dune: option '--root' cannot be repeated
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build --release --root .
  dune: option '--root' cannot be repeated
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

Allowed combinations
--------------------

  $ dune build --release --only-packages toto

  $ dune build --release --only-packages toto --profile foo
