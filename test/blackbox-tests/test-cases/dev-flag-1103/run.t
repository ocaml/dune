jbuilder --dev flag is accepted

  $ jbuilder build --dev
  The jbuilder binary is deprecated and will cease to be maintained in July 2019.
  Please switch to dune instead.

dune --dev flag is rejected

  $ dune build --dev
  dune: --dev is no longer accepted as it is now the default.
  Usage: dune build [OPTION]... [TARGET]...
  Try `dune build --help' or `dune --help' for more information.
  [1]
