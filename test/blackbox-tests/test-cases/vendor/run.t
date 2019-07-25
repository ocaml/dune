Vendored directories should be traversed to find targets so that they are built when they are depend upon

  $ dune build --root duniverse
  Entering directory 'duniverse'

Aliases should not be resolved in vendored sub directories

  $ dune runtest --root duniverse
  Entering directory 'duniverse'
          test alias tests/runtest
  Hello from main lib!

When compiling vendored code, all warnings should be disabled

  $ dune build --root warnings @no-warnings-please 
  Entering directory 'warnings'
  There should be no OCaml warning above!

Dune will not warn about jbuild files within vendored directories

  $ dune build --root jbuild-files @jbuild-are-ok
  Entering directory 'jbuild-files'
  File "vendored/jbuild", line 1, characters 10-20:
  1 | (library ((name lib)))
                ^^^^^^^^^^
  Error: Atom expected
  [1]

Dune will not warn about generating inaccurate .merlin files within vendored directories

  $ dune build --root inaccurate-merlin @inaccurate-merlins-are-ok
  Entering directory 'inaccurate-merlin'
  There should be no inaccurate .merlin warning above!

The vendored_dirs stanza is available from version 1.11 of the dune language

  $ dune build --root from-1-11
  Entering directory 'from-1-11'
  File "dune", line 1, characters 0-17:
  1 | (vendored_dirs *)
      ^^^^^^^^^^^^^^^^^
  Error: 'vendored_dirs' is only available since version 1.11 of the dune
  language
  [1]

The same directory cannot be marked as both vendored and data-only

  $ dune build --root conflicts-with-data-only
  Entering directory 'conflicts-with-data-only'
  Error: Directory dir was marked as vendored and data_only, it can't be marked
  as both.
  [1]
