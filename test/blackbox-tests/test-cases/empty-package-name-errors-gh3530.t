When an empty string is passed to `-p`, we get a nice error message.

  $ echo '(lang dune 2.0)' > dune-project
  $ dune build -p ''
  dune: option '--only-packages': Invalid package name: ""
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]

This can happen in a list as well:

  $ dune build -p 'a,b,'
  dune: option '--only-packages': Invalid package name: ""
  Usage: dune build [OPTION]… [TARGET]…
  Try 'dune build --help' or 'dune --help' for more information.
  [1]
