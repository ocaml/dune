This test just makes sure that dune rules doesn't error out. In the past, we've
had bugs where this subcommand would stop working and nobody would notice as
it's not used very much.

  $ echo "(lang dune 2.5)" > dune-project
  $ dune rules -o Makefile
