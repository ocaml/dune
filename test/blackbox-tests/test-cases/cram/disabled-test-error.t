Test that running a disabled cram test emits a helpful error

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > dune <<EOF
  > (cram
  >  (applies_to disabled)
  >  (enabled_if false))
  > EOF

  $ cat > disabled.t <<EOF
  >   $ echo "this should not run"
  > EOF

Running dune runtest on the directory should succeed (disabled tests are skipped):

  $ dune runtest

Running dune runtest on the specific disabled test should error:

  $ dune runtest disabled.t
  File "dune", lines 1-3, characters 0-49:
  1 | (cram
  2 |  (applies_to disabled)
  3 |  (enabled_if false))
  Error: Test disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in dune:1)
  [1]

Now add an enabled test:

  $ cat > enabled.t <<EOF
  >   $ echo "this should run"
  > EOF

Running both together - disabled errors, enabled runs:

  $ dune runtest enabled.t disabled.t
  File "enabled.t", line 1, characters 0-0:
  Error: Files _build/default/enabled.t and _build/default/enabled.t.corrected
  differ.
  File "dune", lines 1-3, characters 0-49:
  1 | (cram
  2 |  (applies_to disabled)
  3 |  (enabled_if false))
  Error: Test disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in dune:1)
  [1]

Add another disabled test:

  $ cat >> dune <<EOF
  > (cram
  >  (applies_to also-disabled)
  >  (enabled_if false))
  > EOF

  $ cat > also-disabled.t <<EOF
  >   $ echo "this should also not run"
  > EOF

Running both disabled tests together - both errors should show:

  $ dune runtest disabled.t also-disabled.t
  File "dune", lines 1-3, characters 0-49:
  1 | (cram
  2 |  (applies_to disabled)
  3 |  (enabled_if false))
  Error: Test disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in dune:1)
  File "dune", lines 4-6, characters 0-54:
  4 | (cram
  5 |  (applies_to also-disabled)
  6 |  (enabled_if false))
  Error: Test also-disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in dune:4)
  [1]

Add a test with both enabling and disabling stanzas:

  $ cat >> dune <<EOF
  > (cram
  >  (applies_to enabled-and-disabled)
  >  (enabled_if true))
  > (cram
  >  (applies_to enabled-and-disabled)
  >  (enabled_if false))
  > EOF

  $ cat > enabled-and-disabled.t <<EOF
  >   $ echo "what happens here?"
  > EOF

  $ dune runtest enabled-and-disabled.t
  File "dune", lines 10-12, characters 0-61:
  10 | (cram
  11 |  (applies_to enabled-and-disabled)
  12 |  (enabled_if false))
  Error: Test enabled-and-disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in dune:10)
  [1]

Test with a complex false condition:

  $ cat >> dune <<EOF
  > (cram
  >  (applies_to complex-false)
  >  (enabled_if (= 0 1)))
  > EOF

  $ cat > complex-false.t <<EOF
  >   $ echo "complex condition"
  > EOF

  $ dune runtest complex-false.t
  File "dune", lines 13-15, characters 0-56:
  13 | (cram
  14 |  (applies_to complex-false)
  15 |  (enabled_if (= 0 1)))
  Error: Test complex-false.t is disabled.
  The enabled_if condition(s) evaluated to false: (= 0 1) (in dune:13)
  [1]

Test with two disabling stanzas (doubly disabled):

  $ cat >> dune <<EOF
  > (cram
  >  (applies_to doubly-disabled)
  >  (enabled_if false))
  > (cram
  >  (applies_to doubly-disabled)
  >  (enabled_if (= 0 1)))
  > EOF

  $ cat > doubly-disabled.t <<EOF
  >   $ echo "doubly disabled"
  > EOF

  $ dune runtest doubly-disabled.t
  File "dune", lines 19-21, characters 0-58:
  19 | (cram
  20 |  (applies_to doubly-disabled)
  21 |  (enabled_if (= 0 1)))
  Error: Test doubly-disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: (= 0 1) (in dune:19), false
  (in dune:16)
  [1]

Test in a subdirectory to verify path is shown:

  $ mkdir subdir
  $ cat > subdir/dune <<EOF
  > (cram
  >  (applies_to in-subdir)
  >  (enabled_if false))
  > EOF

  $ cat > subdir/in-subdir.t <<EOF
  >   $ echo "in subdir"
  > EOF

  $ dune runtest subdir/in-subdir.t
  File "subdir/dune", lines 1-3, characters 0-50:
  1 | (cram
  2 |  (applies_to in-subdir)
  3 |  (enabled_if false))
  Error: Test subdir/in-subdir.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in subdir/dune:1)
  [1]

Test with :whole_subtree disabled from parent:

  $ mkdir -p subdir2/nested

  $ cat > subdir2/dune <<EOF
  > (cram
  >  (applies_to :whole_subtree)
  >  (enabled_if false))
  > EOF

  $ cat > subdir2/nested/nested-disabled.t <<EOF
  >   $ echo "nested but disabled by parent"
  > EOF

Running dune runtest in subdir2 should succeed (disabled tests skipped):

  $ dune runtest subdir2

Running the specific nested test should error with parent's stanza location:

  $ dune runtest subdir2/nested/nested-disabled.t
  File "subdir2/dune", lines 1-3, characters 0-55:
  1 | (cram
  2 |  (applies_to :whole_subtree)
  3 |  (enabled_if false))
  Error: Test subdir2/nested/nested-disabled.t is disabled.
  The enabled_if condition(s) evaluated to false: false (in subdir2/dune:1)
  [1]
