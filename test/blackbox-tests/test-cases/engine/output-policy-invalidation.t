Changing successful-output policy currently invalidates even a silent rule.
Record the behavior with an ordinary file-producing rule.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ echo '(rule (target result) (action (write-file result done)))' > dune
  $ misses() {
  >   dune trace cat | jq -cs '
  >     [ .[]
  >       | select(.cat == "cache" and .name == "workspace_local_miss")
  >       | select(.args.target | endswith("/result"))
  >       | .args.reason | split(":")[0]
  >     ]
  >   '
  > }
  $ dune build result
  $ dune build result
  $ misses
  []
  $ dune build --action-stdout-on-success=swallow result
  $ misses
  ["rule or dependencies changed"]
  $ dune build --action-stdout-on-success=swallow result
  $ misses
  []
  $ dune build --action-stdout-on-success=swallow \
  >   --action-stderr-on-success=swallow result
  $ misses
  ["rule or dependencies changed"]

must-be-empty is validation, not merely presentation. Any proposed fix must
preserve its checks, even though this particular action produces no output.

  $ dune build --action-stdout-on-success=swallow \
  >   --action-stderr-on-success=must-be-empty result
  $ misses
  ["rule or dependencies changed"]
  $ dune build --action-stdout-on-success=swallow \
  >   --action-stderr-on-success=must-be-empty result
  $ misses
  []
