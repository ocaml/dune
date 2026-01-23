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
  File "foo", line 1, characters 0-0:
  Error: Files _build/default/foo and _build/default/foo.expected differ.
  <one><two>
  [1]

# CR-someday rgrinberg: why isn't this showing anything?!

  $ dune promotion list

  $ dune promote && cat foo
  Promoting _build/default/foo.expected to foo.
   -> ignored _build/default/foo.expected.
   
  two
