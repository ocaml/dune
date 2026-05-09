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
