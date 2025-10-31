Cram tests can forbid git diff3 conflicts:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (cram (conflict_markers error))
  > EOF

Full diff3 conflict without command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<<
  > Left side
  > |||||||
  > Original
  > =======
  > Right side
  > >>>>>>>
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-8, characters 0-73:
  1 | <<<<<<<
  2 | Left side
  3 | |||||||
  4 | Original
  5 | =======
  6 | Right side
  7 | >>>>>>>
  8 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Full diff3 conflict with command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo left
  > |||||||
  >   $ foo original
  > =======
  >   > bar right
  > >>>>>>>
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-8, characters 0-87:
  1 | <<<<<<<
  2 |   $ foo left
  3 | |||||||
  4 |   $ foo original
  5 | =======
  6 |   > bar right
  7 | >>>>>>>
  8 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Partial diff3 conflicts are ignored:

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  >   > bar
  > |||||||
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  > |||||||
  >   > original
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]
