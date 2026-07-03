Failed actions are memoized as reproducible errors. Use an action that increments
a counter before failing to distinguish rerunning the external process from
reraising the cached failure.

In watch mode, a repeated build request for the same failing action reraises the
cached failure without rerunning the action.

  $ make_dune_project 3.22
  $ setup_failing_action_script

  $ counter="$TMPDIR/cached-failed-actions-counter"
  $ rm -f "$counter"
  $ cat > dune <<EOF
  > (alias
  >  (name idle))
  > (rule
  >  (alias fail)
  >  (deps fail.sh)
  >  (action (run sh %{dep:fail.sh} $counter)))
  > EOF

  $ start_dune @idle
  $ build "(alias fail)" > first.out 2>&1
  [1]
  $ count_after_first_build=$(cat "$counter")
  $ build "(alias fail)" > second.out 2>&1
  [1]
  $ test "$(cat "$counter")" = "$count_after_first_build"
  $ stop_dune_quiet

The final build request reused the cached failure, so it executed fewer
processes than the preceding request and did not increment the action counter.

  $ print_build_finish_process_counts
  [
    {
      "outcome": "failure",
      "process_count": 2
    },
    {
      "outcome": "failure",
      "process_count": 1
    }
  ]
  $ test "$(cat "$counter")" = "$count_after_first_build" && echo "counter unchanged"
  counter unchanged
