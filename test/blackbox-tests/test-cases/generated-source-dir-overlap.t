If a generated directory is "overlaid" by a source dir, then things break.

  $ mkdir .dune

  $ cat > .dune/dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo foobar)))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 2.2)
  > EOF

  $ cat > dune <<EOF
  > (dirs .dune)
  > EOF

  $ dune build @foo
  Error: Alias "foo" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Hint: did you mean fmt?
  [1]

The command above claims that @foo isn't defined, but it clearly is if we
replace .dune with a normal name.
