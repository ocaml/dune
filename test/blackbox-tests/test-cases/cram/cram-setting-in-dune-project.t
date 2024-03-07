Test the behavior when there are cram stanzas but cram tests are not enabled.

We need a sub-directory, otherwise the inner dune will see this run.t
file and we enter a loop:

  $ mkdir test
  $ cd test

  $ cat >dune-project<<EOF
  > (lang dune 2.8)
  > EOF

  $ cat >dune<<EOF
  > (cram)
  > EOF

  $ cat >run.t<<EOF
  >   $ echo "Hello, world!"
  > EOF

With older version of Dune, this would do nothing because of the
missing (cram enable) in the dune-project file:

  $ dune runtest
  File "dune", line 1, characters 0-6:
  1 | (cram)
      ^^^^^^
  Warning: Cram tests are not enabled in this project.
  Hint: You can enable cram tests by adding (cram enable) to your dune-project
  file.

Check that once we enable cram tests, the test are indeed being
executed:

  $ echo "(cram enable)" >> dune-project
  $ dune runtest
  File "run.t", line 1, characters 0-0:
  Error: Files _build/default/run.t and _build/default/run.t.corrected differ.
  [1]

With Dune 3.0 and later, we don't get an error since cram tests are enabled by
default:

  $ cat >dune-project<<EOF
  > (lang dune 3.0)
  > EOF

  $ dune runtest
  File "run.t", line 1, characters 0-0:
  Error: Files _build/default/run.t and _build/default/run.t.corrected differ.
  [1]

And if we disable them on purpose, we get an error message:

  $ echo "(cram disable)" >> dune-project
  $ dune runtest
  File "dune", line 1, characters 0-6:
  1 | (cram)
      ^^^^^^
  Error: Cram tests are not enabled in this project.
  Hint: You can enable cram tests by adding (cram enable) to your dune-project
  file.
  [1]
