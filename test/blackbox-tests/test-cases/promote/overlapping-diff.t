Test how overlapping diff actions are handled

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ touch foo

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (echo "<one>")
  >    (no-infer (with-stdout-to foo.expected (echo one)))
  >    (diff? foo foo.expected))))
  > (rule
  >  (alias foo)
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (echo "<two>")
  >    (no-infer (with-stdout-to foo.expected (echo two)))
  >    (diff? foo foo.expected))))
  > EOF

  $ dune build @foo
  <one><two>File "foo", line 1, characters 0-0:
  --- foo
  +++ foo.expected
  @@ -0,0 +1 @@
  +one
  \ No newline at end of file
  File "foo", line 1, characters 0-0:
  --- foo
  +++ foo.expected
  @@ -0,0 +1 @@
  +two
  \ No newline at end of file
  [1]

  $ dune trace cat | jq 'select(.cat == "promote") | .args'
  {
    "src": "_build/default/foo.expected",
    "dst": "foo",
    "how": "staged"
  }
  {
    "src": "_build/default/foo.expected",
    "dst": "foo",
    "how": "staged"
  }

  $ dune promotion list
  foo
  foo

  $ dune promote && cat foo
  Promoting _build/default/foo.expected to foo.
   -> ignored _build/default/foo.expected.
   
  two
