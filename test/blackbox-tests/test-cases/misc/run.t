  $ dune runtest --display short
          diff alias runtest
          diff alias runtest

Testing multiline commands in cram tests:
  $ cat <<EOF
  > Multiline
  > Text
  > EOF
  Multiline
  Text

Test that incompatible options are properly reported
----------------------------------------------------

  $ dune build --verbose --display quiet
  dune: Cannot use --verbose and --display simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build -p toto --root .
  dune: Cannot use --root and -p simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build --for-release-of-packages toto --root .
  dune: Cannot use --root and --for-release-of-packages simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]

  $ dune build --no-config --config x
  dune: Cannot use --config and --no-config simultaneously
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]
