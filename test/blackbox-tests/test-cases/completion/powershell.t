PowerShell completion scripts fall back to filename completion when Dune has no
semantic completions to offer.

This test sources the generated PowerShell completion script and exercises the
completion it registered for dune.

  $ mkdir completions
  $ touch completions/powershell.t completions/other-file
  $ cd completions
  $ dune completion powershell > dune.ps1

Subcommand names are completed semantically:

  $ pwsh -NoLogo -NoProfile -NonInteractive -Command '
  > . ./dune.ps1
  > (TabExpansion2 "dune runtes" 11).CompletionMatches |
  >   ForEach-Object CompletionText
  > '
  runtest

Positional arguments without semantic completions fall back to filename
completion:

  $ pwsh -NoLogo -NoProfile -NonInteractive -Command '
  > . ./dune.ps1
  > (TabExpansion2 "dune runtest pow" 16).CompletionMatches |
  >   ForEach-Object { Split-Path -Leaf $_.CompletionText }
  > '
  powershell.t
