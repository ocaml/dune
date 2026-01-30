Dune can emit events for threads started

  $ make_dune_project 3.21

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo "")))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq -c 'select(.cat == "thread") | .args'
  {"name":"signal-watcher"}
