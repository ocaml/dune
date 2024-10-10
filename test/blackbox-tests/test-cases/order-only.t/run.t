This test ensures that dependencies defined with 'order_only'
specification do not retrigger the concerned rule.

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat >a <<EOF
  > contents of a
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps (order_only (file a)))
  >  (action
  >  (echo "executing foo")))
  > EOF

  $ dune build @foo
  executing foo

  $ cat >a <<EOF
  > contents of a changed
  > EOF

  $ dune build
