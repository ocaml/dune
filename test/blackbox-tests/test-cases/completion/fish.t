Fish completion scripts fall back to filename completion when Dune has no
semantic completions to offer.

This test sources the generated fish completion script and exercises the
completion it registered for dune.

  $ mkdir completions
  $ touch completions/fish.t completions/other-file
  $ cd completions
  $ dune completion fish > dune.fish

Subcommand names are completed semantically:

  $ fish --no-config -c 'source dune.fish; complete -C"dune runtes"' | cut -f1
  runtest

Positional arguments without semantic completions fall back to filename
completion:

  $ fish --no-config -c 'source dune.fish; complete -C"dune runtest fis"'
  fish.t
