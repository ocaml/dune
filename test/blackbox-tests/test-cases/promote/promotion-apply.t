`dune promotion run` is equivalent to `dune promote`.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (diff a.expected a.actual)))
  > 
  > (rule
  >  (write-file a.actual Actual))
  > EOF

  $ cat > a.expected << EOF
  > Expected
  > EOF

  $ dune runtest
  File "a.expected", line 1, characters 0-0:
  Error: Files _build/default/a.expected and _build/default/a.actual differ.
  [1]
  $ cat a.expected
  Expected
  $ dune promotion apply
  Promoting _build/default/a.actual to a.expected.
  $ cat a.expected
  Actual
