  $ dune runtest --display short
          diff alias runtest
          diff alias runtest

Test that incompatible options are properly reported
----------------------------------------------------

  $ dune build --verbose --display quiet
  dune build: Cannot use --verbose and --display simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build -p toto --root .
  dune build: option `--root' cannot be repeated
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build --for-release-of-packages toto --root .
  dune build: option `--root' cannot be repeated
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build --no-config --config x
  dune build: Cannot use --config and --no-config simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build -p toto --release
  dune build: option `--root' cannot be repeated
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build --release --root .
  dune build: option `--root' cannot be repeated
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

Allowed combinations
--------------------

  $ dune build --release --only-packages toto
  Warning: The package toto does not have any user defined stanzas attached to
  it. If this is intentional, add (allow_empty) to the package definition in
  the dune-project file

  $ dune build --release --only-packages toto --profile foo
  Warning: The package toto does not have any user defined stanzas attached to
  it. If this is intentional, add (allow_empty) to the package definition in
  the dune-project file
