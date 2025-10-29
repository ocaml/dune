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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
