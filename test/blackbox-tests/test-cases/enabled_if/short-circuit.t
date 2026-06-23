Test that `and` and `or` in enabled_if should short-circuit.

When the first conjunct of `and` is false, the second should not be evaluated.

  $ make_dune_project 3.0

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

  $ dune build @foo

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
  this should run
