Test behavior of disabled cram tests.

The enabled_if field controls whether a test is included in @runtest.
Currently, disabled tests silently succeed when run explicitly - the test
simply doesn't run but no error is reported.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (applies_to disabled)
  >  (enabled_if false))
  > EOF

Create a disabled test with wrong expected output - if it runs, we'd see a diff:

  $ cat > disabled.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

Running dune runtest on the directory succeeds (disabled tests are skipped):

  $ dune runtest

Running dune runtest on the specific disabled test silently succeeds.
No diff is shown, proving the test did not actually run:

  $ dune runtest disabled.t

Now add an enabled test (also with wrong output to show it runs):

  $ cat > enabled.t <<EOF
  >   $ echo "hello"
  >   wrong output
  > EOF

Running both together - enabled shows diff (it ran), disabled shows nothing:

  $ dune runtest enabled.t disabled.t
  File "enabled.t", line 1, characters 0-0:
  Error: Files _build/default/enabled.t and _build/default/enabled.t.corrected
  differ.
  [1]

Test conjunction semantics: a test with both enabling and disabling stanzas
is disabled (all enabled_if conditions must be true):

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

No diff shown - test is disabled due to conjunction of conditions:

  $ dune runtest mixed.t

Test :whole_subtree inheritance - parent directory can disable tests in children:

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

Running dune runtest in subdir succeeds (disabled tests skipped):

  $ dune runtest subdir

No diff shown - nested test is disabled by parent's :whole_subtree stanza:

  $ dune runtest subdir/nested/nested.t
