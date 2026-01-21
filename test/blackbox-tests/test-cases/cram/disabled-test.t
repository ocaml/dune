Test that disabled cram tests can still be run explicitly.

The enabled_if field controls whether a test is included in @runtest,
but does not prevent the test from being run explicitly.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (applies_to disabled)
  >  (enabled_if false))
  > EOF

Create a disabled test with wrong expected output - if it runs, we see a diff:

  $ cat > disabled.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

Running dune runtest on the directory succeeds (disabled tests are skipped):

  $ dune runtest

Running dune runtest on the specific disabled test now runs it.
The diff proves the test actually executed:

  $ dune runtest disabled.t
  File "disabled.t", line 1, characters 0-0:
  Error: Files _build/default/disabled.t and
  _build/default/disabled.t.corrected differ.
  [1]

Now add an enabled test (also with wrong output to show it runs):

  $ cat > enabled.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

Running both together - both show diffs (both ran):

  $ dune runtest enabled.t disabled.t
  File "disabled.t", line 1, characters 0-0:
  Error: Files _build/default/disabled.t and
  _build/default/disabled.t.corrected differ.
  File "enabled.t", line 1, characters 0-0:
  Error: Files _build/default/enabled.t and _build/default/enabled.t.corrected
  differ.
  [1]

Test conjunction semantics: a test with both enabling and disabling stanzas
is disabled from @runtest but can still be run explicitly:

  $ cat >> dune <<EOF
  > (cram
  >  (applies_to mixed)
  >  (enabled_if true))
  > (cram
  >  (applies_to mixed)
  >  (enabled_if false))
  > EOF

  $ cat > mixed.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

The diff proves the test ran when explicitly requested:

  $ dune runtest mixed.t
  File "mixed.t", line 1, characters 0-0:
  Error: Files _build/default/mixed.t and _build/default/mixed.t.corrected
  differ.
  [1]

Test :whole_subtree inheritance - parent directory can disable tests in children
from @runtest, but they can still be run explicitly:

  $ mkdir -p subdir/nested

  $ cat > subdir/dune <<EOF
  > (cram
  >  (applies_to :whole_subtree)
  >  (enabled_if false))
  > EOF

  $ cat > subdir/nested/nested.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

Running dune runtest in subdir succeeds (disabled tests skipped from alias):

  $ dune runtest subdir

The diff proves the nested test ran when explicitly requested:

  $ dune runtest subdir/nested/nested.t
  File "subdir/nested/nested.t", line 1, characters 0-0:
  Error: Files _build/default/subdir/nested/nested.t and
  _build/default/subdir/nested/nested.t.corrected differ.
  [1]
