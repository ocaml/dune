SIGHUP should follow normal scheduler shutdown and preserve the workspace
database. A completed rule must remain cached after a hangup during other work.

After sending SIGHUP, the action blocks on a FIFO with no writer. Only
scheduler shutdown can end the action, so completion cannot race with SIGHUP.

  $ export DUNE_TRACE=+cache
  $ make_dune_project 3.26
  $ cat > dune <<EOF
  > (rule (target completed) (action (write-file completed done)))
  > (rule
  >  (target hung-up)
  >  (deps completed)
  >  (action
  >   (run sh -c "mkfifo block && kill -HUP \$(cat '$PWD/_build/.lock') &&
  >               read -r line < block")))
  > EOF
  $ dune build completed
  $ test -f _build/.db
  $ ($timeout -k 5 30 dune build hung-up >hangup.log 2>&1; echo $?) 2>/dev/null
  130
  $ test -f _build/.db
  $ test -f _build/default/completed
  $ dune build completed
  $ dune trace cat | jq -cs '
  >   [ .[]
  >     | select(.cat == "cache" and .name == "workspace_local_miss")
  >     | select(.args.target | endswith("/completed"))
  >     | .args.reason | split(":")[0]
  >   ]
  > '
  []
