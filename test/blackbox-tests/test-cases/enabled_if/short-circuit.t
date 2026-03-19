Test that `and` and `or` in enabled_if should short-circuit.

When the first conjunct of `and` is false, the second should not be evaluated.
Currently this is broken: both are evaluated eagerly, causing spurious errors.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (enabled_if
  >   (and
  >    %{lib-available:nonexistent-library}
  >    (>= %{version:nonexistent-library} 1.0)))
  >  (action (echo "this should not run")))
  > EOF

The rule should simply be disabled (lib-available is false), not error out.
But due to missing short-circuit evaluation, version expansion fails:

  $ dune build @foo
  File "dune", line 6, characters 7-37:
  6 |    (>= %{version:nonexistent-library} 1.0)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package "nonexistent-library" doesn't exist in the current project and
  isn't installed either.
  [1]

Similarly, `or` should short-circuit when the first disjunct is true:

  $ cat >dune <<EOF
  > (rule
  >  (alias bar)
  >  (enabled_if
  >   (or
  >    true
  >    (>= %{version:nonexistent-library} 1.0)))
  >  (action (echo "this should run")))
  > EOF

  $ dune build @bar
  File "dune", line 6, characters 7-37:
  6 |    (>= %{version:nonexistent-library} 1.0)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Package "nonexistent-library" doesn't exist in the current project and
  isn't installed either.
  [1]
