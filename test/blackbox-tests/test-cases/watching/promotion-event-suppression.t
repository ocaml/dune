Promotion in watch mode should not retrigger rules unnecessarily.

File-system events caused by promotion should not run the promotion rule twice.
Same-content events for promoted source files should not rerun dependent rules,
but real content changes should still be restored by promotion.

  $ write_promoted_result_project \
  >   "cp original promoted; printf 'promote\n' >> ../runs" \
  >   "cat promoted > result; printf 'result\n' >> ../runs"
  $ echo one > original
  $ echo stale > promoted
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ touch promoted
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ echo changed > promoted
  $ build result
  Success
  $ cat promoted
  one
  $ cat _build/runs
  promote
  result
  $ stop_dune > /dev/null

Artifact substitution changes the promoted bytes without changing the build
artifact. Self-generated events must not promote it again.

  $ mkdir artifact-substitution
  $ cd artifact-substitution
  $ make_dune_project 3.0
  $ cat > dune <<EOF
  > (alias (name idle))
  > (rule
  >  (target promoted)
  >  (deps input)
  >  (mode promote)
  >  (action (copy input promoted)))
  > EOF
  $ printf '%s' '%%DUNE_PLACEHOLDER:36:repeat:1:1:x%%' > input
  $ echo stale > promoted
  $ export DUNE_TRACE=promote
  $ start_dune @idle
  $ build promoted
  Success
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build promoted
  Success
  $ stop_dune_quiet
  $ dune trace cat | jq -s '[.[] | select(.name == "promote")] | length'
  1
  $ head -c 4 promoted; echo
  =1:x
