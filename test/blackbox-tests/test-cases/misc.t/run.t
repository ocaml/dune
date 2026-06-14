  $ dune runtest --display short
          diff alias runtest
  File "dune", lines 17-20, characters 0-91:
  17 | (alias
  18 |  (name   runtest)
  19 |  (deps   result expected)
  20 |  (action (run diff -u result expected)))
          diff alias runtest (exit 1)
  --- result	2026-06-12 15:55:16.510796208 +0100
  +++ expected	2026-06-12 15:55:16.510796208 +0100
  @@ -1 +1 @@
  -./dune ./a.txt ./b.txt ./c.txt
  \ No newline at end of file
  +dune a.txt b.txt c.txt
  \ No newline at end of file
  [1]

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
