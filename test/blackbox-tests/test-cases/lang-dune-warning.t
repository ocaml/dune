If (lang dune) does not use a valid format on a 2.x project, a warning is
emitted:

  $ cat > dune-project << EOF
  > (lang dune 2.3.0)
  > EOF

  $ dune build
  File "dune-project", line 1, characters 11-16:
  1 | (lang dune 2.3.0)
                 ^^^^^
  Warning: The ".0" part is ignored here.
  This version is parsed as just 2.3.

  $ cat > dune-project << EOF
  > (lang dune 2.4suffix)
  > EOF

  $ dune build
  File "dune-project", line 1, characters 11-20:
  1 | (lang dune 2.4suffix)
                 ^^^^^^^^^
  Warning: The "suffix" part is ignored here.
  This version is parsed as just 2.4.

If the version is valid, no warning is emitted:

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

  $ dune build

Starting with lang 3.0, the warning turns into an error.

  $ cat > dune-project << EOF
  > (lang dune 3.0suffix)
  > EOF

  $ dune build
  File "dune-project", line 1, characters 11-20:
  1 | (lang dune 3.0suffix)
                 ^^^^^^^^^
  Error: The "suffix" part is ignored here.
  This version is parsed as just 3.0.
  [1]

And without suffix it is accepted.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

  $ dune build

If a new version of dune lang is encountered, we print a helpful error message
to the user:

  $ cat > dune-project << EOF
  > (lang dune 123.123)
  > EOF

  $ dune build 2>&1 | sed -E 's/ 3.[0-9]+/ 3.XX/g'
  File "dune-project", line 1, characters 11-18:
  1 | (lang dune 123.123)
                 ^^^^^^^
  Error: Version 123.123 of the dune language is not supported.
  Supported versions of the dune language in version 3.XX of dune are:
  - 1.0 to 1.12
  - 2.0 to 2.9
  - 3.XX to 3.XX
