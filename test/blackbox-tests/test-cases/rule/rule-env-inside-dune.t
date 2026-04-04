Demonstrate that INSIDE_DUNE can be observed by a rule:

  $ make_dune_project 3.23

  $ cat >dune <<'EOF'
  > (rule
  >  (alias foo)
  >  (action (bash "echo $INSIDE_DUNE")))
  > EOF

  $ dune build @foo
  $TESTCASE_ROOT/_build/default
