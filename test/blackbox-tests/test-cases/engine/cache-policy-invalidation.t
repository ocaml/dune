Shared-cache eligibility must not change a rule's identity. Currently enabling
it invalidates an otherwise unchanged rule, even after a successful null build.

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
  $ DUNE_CACHE=disabled dune build result
  $ DUNE_CACHE=disabled dune build result
  $ misses
  []
  $ DUNE_CACHE=enabled dune build result
  $ misses
  ["rule or dependencies changed"]
  $ DUNE_CACHE=enabled dune build result
  $ misses
  []
  $ DUNE_CACHE=disabled dune build result
  $ misses
  ["rule or dependencies changed"]
