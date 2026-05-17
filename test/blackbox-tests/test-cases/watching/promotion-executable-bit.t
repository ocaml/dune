Promotion up-to-date checks in watch mode must include the executable bit, not
just file contents.

The second build repairs the promoted source file's permissions without rerunning
the rule action.

  $ write_promoted_result_project \
  >   "cp original promoted; chmod 655 promoted; printf 'promote\n' >> ../runs" \
  >   "dune_cmd stat permissions promoted > result; cat result >> ../runs"
  $ echo one > original
  $ echo one > promoted
  $ chmod 644 promoted
  $ start_dune @idle
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ chmod 644 promoted
  $ touch promoted
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ stop_dune > /dev/null
