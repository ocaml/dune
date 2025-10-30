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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
  Error: Conflict marker found. Please remove it or set (conflict_markers
  allow)
  -> required by _build/default/.cram.test.t/cram.sh
  -> required by _build/default/.cram.test.t/cram.out
  -> required by alias test
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
