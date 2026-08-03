This test just makes sure that dune rules doesn't error out. In the past, we've
had bugs where this subcommand would stop working and nobody would notice as
it's not used very much.

  $ make_dune_project 2.5
  $ dune rules --root . --format=json -o Makefile
