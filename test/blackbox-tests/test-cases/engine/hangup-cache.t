SIGHUP during unrelated work currently loses the workspace database and reruns
a completed rule with unchanged inputs and the same rule key. Unlike SIGKILL,
this signal is catchable and should preserve the cache through normal shutdown.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ cat > dune <<EOF
  > (rule (target completed) (action (write-file completed done)))
  > (rule
  >  (target hung-up)
  >  (deps completed)
  >  (action (run sh -c "kill -HUP \$(cat $PWD/_build/.lock)")))
  > EOF
  $ rule_key() {
  >   dune trace cat | jq -r '
  >     select(.cat == "cache" and .name == "miss")
  >     | select(.args.head | endswith("/completed"))
  >     | .args.rule_digest | strings
  >   '
  > }
  $ dune build completed
  $ test -f _build/.db
  $ first_key=$(rule_key)
  $ test -n "$first_key"
  $ (dune build hung-up > hung-up-output 2>&1; echo $?) 2>/dev/null
  129
  $ test -f _build/.db
  [1]
  $ test -f _build/default/completed
  $ dune build completed
  $ dune trace cat | jq -cs '
  >   [ .[]
  >     | select(.cat == "cache" and .name == "workspace_local_miss")
  >     | select(.args.target | endswith("/completed"))
  >     | .args.reason | split(":")[0]
  >   ]
  > '
  ["never seen this target before"]
  $ test "$first_key" = "$(rule_key)"
