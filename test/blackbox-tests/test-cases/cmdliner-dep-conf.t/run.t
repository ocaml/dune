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
  dune: TARGET… arguments: unclosed parenthesis at end of input
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

  $ dune build "()"
  dune: TARGET… arguments: Unexpected list
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]
