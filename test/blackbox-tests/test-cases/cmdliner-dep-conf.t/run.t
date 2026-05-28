Tests TARGET argument parsing for dependency configurations.

  $ dune build "(alias foo)"
  Hello!

  $ dune build foo.txt
  $ cat foo.txt
  Foo

  $ dune build "(file @bar)"
  $ cat @bar
  Bar

  $ dune build "(source_tree foo)"
  Error: Don't know how to build "(source_tree foo)"
  [1]

  $ dune build "(fi"
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: TARGET… arguments: unclosed parenthesis at end of input
  [1]

  $ dune build "()"
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: TARGET… arguments: Unexpected list
  [1]
