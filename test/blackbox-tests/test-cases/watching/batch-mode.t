Failed actions are memoized as reproducible errors. Use an action that increments
a counter before failing to distinguish rerunning the external process from
reraising the cached failure.

In a non-watch build, requesting the same failing action through two aliases in a
single build executes the failed action only once.

  $ make_dune_project 3.22
  $ setup_failing_action_script

  $ counter="$TMPDIR/cached-failed-actions-counter"
  $ rm -f "$counter"
  $ cat > dune <<EOF
  > (rule
  >  (alias fail)
  >  (deps fail.sh)
  >  (action (run sh %{dep:fail.sh} $counter)))
  > (alias
  >  (name a)
  >  (deps (alias fail)))
  > (alias
  >  (name b)
  >  (deps (alias fail)))
  > EOF

  $ dune build @a @b > build.out 2>&1
  [1]
  $ cat "$counter"
  1
  $ print_build_finish_process_counts
  [
    {
      "outcome": "failure",
      "process_count": 2
    }
  ]
