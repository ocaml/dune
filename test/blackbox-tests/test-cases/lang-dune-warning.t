If (lang dune) does not use a valid format, a warning is emitted:

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

Of course if the version is valid, no warning is emitted:

  $ cat > dune-project << EOF
  > (lang dune 2.2)
  > EOF

  $ dune build
