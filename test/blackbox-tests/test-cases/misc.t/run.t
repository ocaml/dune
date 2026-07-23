  $ dune runtest --display short
          diff alias runtest
          diff alias runtest

Test that incompatible options are properly reported
----------------------------------------------------

  $ dune build --verbose --display quiet
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: Cannot use --verbose and --display simultaneously
  [1]

  $ dune build -p toto --root .
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--root' cannot be repeated
  [1]

  $ dune build --for-release-of-packages toto --root .
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--root' cannot be repeated
  [1]

  $ dune build --no-config --config x
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: Cannot use --config and --no-config simultaneously
  [1]

  $ dune build -p toto --release
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--root' cannot be repeated
  [1]

  $ dune build --release --root .
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--root' cannot be repeated
  [1]

Allowed combinations
--------------------

  $ dune build --release --only-packages toto

  $ dune build --release --only-packages toto --profile foo
