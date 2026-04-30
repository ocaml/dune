Alias hint generation should only use the selected build context in a
multi-context workspace. Aliases from other build contexts should not appear
as hints.

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.17)
  > (context default)
  > (context (default (name other)))
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias foo)
  >  (action (echo "foo"))
  >  (enabled_if (= %{context_name} "default")))
  > (rule
  >  (alias foe)
  >  (action (echo "foe"))
  >  (enabled_if (= %{context_name} "other")))
  > EOF

We have "foo" in the default context and "foe" in the "other" context. When
building a misspelled alias "@fou", hints should only come from the default
context, so only "foo" (not "foe") should be suggested.

  $ dune build @fou
  Error: Alias "fou" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Hint: did you mean fmt or foo?
  [1]
