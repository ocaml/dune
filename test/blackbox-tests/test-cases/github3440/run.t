This test just makes sure that dune rules doesn't error out. In the past, we've
had bugs where this subcommand would stop working and nobody would notice as
it's not used very much.

  $ echo "(lang dune 2.5)" > dune-project
  $ dune rules -o Makefile
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Fdecl.get: not set", {})
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/stdune/path.ml", line 833, characters 26-60
  Called from file "src/stdune/option.ml", line 9, characters 21-26
  Called from file "bin/print_rules.ml", line 110, characters 12-44
  Called from file "vendor/cmdliner/src/cmdliner_term.ml", line 25, characters
    19-24
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 146, characters 9-16
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 176, characters
    18-36
  Called from file "vendor/cmdliner/src/cmdliner.ml", line 312, characters
    20-46
  Called from file "bin/main.ml", line 261, characters 10-51
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
