When an empty string is passed to `-p`, we get a nice error message.

  $ echo '(lang dune 2.0)' > dune-project
  $ dune build -p ''
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--only-packages': Invalid package name: ""
  [1]

This can happen in a list as well:

  $ dune build -p 'a,b,'
  Usage: dune build [--help] [OPTION]… [TARGET]…
  dune: option '--only-packages': Invalid package name: ""
  [1]
