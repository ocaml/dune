If (lang dune) does not use a valid format on a 2.x project, a warning is
emitted:

  $ make_dune_project 2.3.0

  $ dune build
  File "dune-project", line 1, characters 11-16:
  1 | (lang dune 2.3.0)
                 ^^^^^
  Warning: The ".0" part is ignored here.
  This version is parsed as just 2.3.

  $ make_dune_project 2.4suffix

  $ dune build
  File "dune-project", line 1, characters 11-20:
  1 | (lang dune 2.4suffix)
                 ^^^^^^^^^
  Warning: The "suffix" part is ignored here.
  This version is parsed as just 2.4.

If the version is valid, no warning is emitted:

  $ make_dune_project 2.2

  $ dune build

Starting with lang 3.0, the warning turns into an error.

  $ make_dune_project 3.0suffix

  $ dune build
  File "dune-project", line 1, characters 11-20:
  1 | (lang dune 3.0suffix)
                 ^^^^^^^^^
  Error: The "suffix" part is ignored here.
  This version is parsed as just 3.0.
  [1]

And without suffix it is accepted.

  $ make_dune_project 3.0

  $ dune build
