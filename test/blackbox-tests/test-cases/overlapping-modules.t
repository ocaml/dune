When executables have overlapping modules we should tell the user this is the
case.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name baz))
  > (executable
  >  (name bar))
  > EOF
  $ cat > baz.ml
  $ cat > bar.ml

Currently this check is missing for overlapping executable(s) stanzas.

  $ dune build @check
  File "dune", line 1, characters 0-0:
  Error: Module "Baz" is used in several stanzas:
  - dune:1
  - dune:3
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

Overlapping executable and library:

  $ cat > dune <<EOF
  > (executable
  >  (name baz))
  > (library
  >  (name bar))
  > EOF

  $ dune build @check
  File "dune", line 1, characters 0-0:
  Error: Module "Baz" is used in several stanzas:
  - dune:1
  - dune:3
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

Overlapping libraries:

  $ cat > dune <<EOF
  > (library
  >  (name baz))
  > (library
  >  (name bar))
  > EOF

  $ dune build @check
  File "dune", line 1, characters 0-0:
  Error: Module "Baz" is used in several stanzas:
  - dune:1
  - dune:3
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

