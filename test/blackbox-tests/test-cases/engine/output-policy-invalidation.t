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

A build with `--no-buffer` does not check the subprocess's stdout. Its output
should not satisfy a subsequent buffered build that requires stdout to be empty.

  $ cat > dune <<'EOF'
  > (rule
  >  (target noisy)
  >  (action (system "echo unexpected; touch noisy")))
  > EOF
  $ dune build --cache=disabled --no-buffer --action-stdout-on-success=must-be-empty noisy
  unexpected

CR: The buffered build uses the workspace-local result of the unbuffered
build instead of validating stdout.

  $ dune build --cache=disabled --action-stdout-on-success=must-be-empty noisy
  $ test -f _build/default/noisy

A fresh buffered build rejects the same action.

  $ dune build --cache=disabled --build-dir _fresh \
  >   --action-stdout-on-success=must-be-empty noisy > fresh.log 2>&1
  [1]
  $ grep '^unexpected$' fresh.log
  unexpected
