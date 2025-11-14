Cram tests can forbid jujutsu conflicts:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (cram (conflict_markers error))
  > EOF

Full jujutsu conflict without command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<< Conflict 1 of 2
  > A Small
  > %%%%%%%
  > Conflict
  > >>>>>>> Conflict 1 of 2 ends
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-6, characters 0-89:
  1 | <<<<<<< Conflict 1 of 2
  2 | A Small
  3 | %%%%%%%
  4 | Conflict
  5 | >>>>>>> Conflict 1 of 2 ends
  6 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Full jujutsu conflict with command and output interleaving:

  $ cat >test.t <<EOF
  > <<<<<<< Conflict 2 of 2
  >   $ foo
  > %%%%%%%
  >   > bar
  > >>>>>>> Conflict 2 of 2 ends
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-6, characters 0-88:
  1 | <<<<<<< Conflict 2 of 2
  2 |   $ foo
  3 | %%%%%%%
  4 |   > bar
  5 | >>>>>>> Conflict 2 of 2 ends
  6 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Jujutsu default style conflict (diff + snapshot):

  $ cat >test.t <<EOF
  > <<<<<<< Conflict 1 of 1
  > %%%%%%% Changes from base to side #1
  > -apple
  > +grapefruit
  > +++++++ Contents of side #2
  > APPLE
  > GRAPE
  > >>>>>>> Conflict 1 of 1 ends
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-9, characters 0-160:
  1 | <<<<<<< Conflict 1 of 1
  2 | %%%%%%% Changes from base to side #1
  3 | -apple
  4 | +grapefruit
  5 | +++++++ Contents of side #2
  6 | APPLE
  7 | GRAPE
  8 | >>>>>>> Conflict 1 of 1 ends
  9 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Jujutsu snapshot style conflict:

  $ cat >test.t <<EOF
  > <<<<<<< Conflict 1 of 1
  > +++++++ Contents of side #1
  > Left side
  > ------- Contents of base
  > Original
  > +++++++ Contents of side #2
  > Right side
  > >>>>>>> Conflict 1 of 1 ends
  > $ echo tada
  > EOF

  $ dune runtest test.t
  File "test.t", lines 1-9, characters 0-175:
  1 | <<<<<<< Conflict 1 of 1
  2 | +++++++ Contents of side #1
  3 | Left side
  4 | ------- Contents of base
  5 | Original
  6 | +++++++ Contents of side #2
  7 | Right side
  8 | >>>>>>> Conflict 1 of 1 ends
  9 | $ echo tada
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  [1]

Partial jujutsu conflicts are ignored:

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  >   > bar
  > >>>>>>>
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ cat >test.t <<EOF
  > <<<<<<<
  >   $ foo
  > %%%%%%%
  >   > bar
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]
