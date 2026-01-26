Dune can emit events for threads started

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (echo "")))
  > EOF

  $ dune build @foo

  $ dune trace cat | jq -c 'select(.cat == "thread") | .args'
  {"name":"signal-watcher"}
