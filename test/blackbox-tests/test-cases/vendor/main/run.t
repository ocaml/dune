Vendored directories should be traversed to find targets so that they are built when they are depend upon

  $ dune build --root duniverse --debug-dependency-path
  Entering directory 'duniverse'

Aliases should not be resolved in vendored sub directories

  $ dune runtest --root duniverse
  Entering directory 'duniverse'
          test alias tests/runtest
  Hello from main lib!

When compiling vendored code, all warnings should be disabled

  $ dune build --root warnings @no-warnings-please
  Entering directory 'warnings'
  There should be no OCaml warning!

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
  language. Please update your dune-project file to have (lang dune 1.11).
  [1]

The same directory cannot be marked as both vendored and data-only

  $ dune build --root conflicts-with-data-only
  Entering directory 'conflicts-with-data-only'
  Error: Directory dir was marked as vendored and data_only, it can't be marked
  as both.
  [1]

The current directory cannot be marked as vendored

  $ dune build --root self-vendored
  Entering directory 'self-vendored'
  File "dune", line 1, characters 15-16:
  1 | (vendored_dirs .)
                     ^
  Error: invalid sub-directory name "."
  Hint: did you mean (vendored_dirs *)?
  [1]

The current directory cannot be marked as data-only

  $ dune build --root self-data-only
  Entering directory 'self-data-only'
  File "dune", line 1, characters 16-17:
  1 | (data_only_dirs .)
                      ^
  Error: invalid sub-directory name "."
  Hint: did you mean (data_only *)?
  [1]

Only direct subdirectories can be marked as vendored

  $ dune build --root deep-subfolder-vendor
  Entering directory 'deep-subfolder-vendor'
  File "dune", line 1, characters 15-18:
  1 | (vendored_dirs a/b)
                     ^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore a/b, write "(vendored_dirs b)" in a/dune
  [1]

Only direct subdirectories can be marked as data-only

  $ dune build --root deep-subfolder-dataonly
  Entering directory 'deep-subfolder-dataonly'
  File "dune", line 1, characters 16-21:
  1 | (data_only_dirs a/b/c)
                      ^^^^^
  Error: only immediate sub-directories may be specified.
  Hint: to ignore a/b/c, write "(data_only c)" in a/b/dune
  [1]

Multiple direct subdirectories can be marked as data-only or vendored

  $ dune build --root multiple-dirs
  Entering directory 'multiple-dirs'

