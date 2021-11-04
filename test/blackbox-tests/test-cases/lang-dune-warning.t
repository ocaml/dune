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
