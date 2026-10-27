Basic tests for the file-watching mode.

----------------------------------------------------------------------------------
* Compile a simple rule

  $ make_dune_project 2.0

  $ cat > x <<EOF
  > original-contents
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target y)
  >  (deps x)
  >  (action (system "cat x > y")))
  > EOF

  $ start_dune

  $ build y
  Success
  $ cat _build/default/y
  original-contents

  $ echo new-contents > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents

  $ echo new-contents2 > x

  $ build y
  Success
  $ cat _build/default/y
  new-contents2

----------------------------------------------------------------------------------
* File rename

  $ mv x z
  $ build y
  Failure
  [1]

  $ echo new-contents3 > z

  $ build y
  Failure
  [1]

  $ mv z x
  $ build y
  Success
  $ cat _build/default/y
  new-contents3

  $ with_timeout dune shutdown
  $ cat .#dune-output
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  File "dune", lines 1-4, characters 0-59:
  1 | (rule
  2 |  (target y)
  3 |  (deps x)
  4 |  (action (system "cat x > y")))
  Error: No rule found for x
  Had 1 error, waiting for filesystem changes...
  File "dune", lines 1-4, characters 0-59:
  1 | (rule
  2 |  (target y)
  3 |  (deps x)
  4 |  (action (system "cat x > y")))
  Error: No rule found for x
  Had 1 error, waiting for filesystem changes...
  Success, waiting for filesystem changes...

  $ wait_for_dune_exit

------------------------------------------------------------------------------
* Sibling directory targets

One rule produces both directory targets. Changing one directory's contents must
only rerun its glob consumer, not the consumer of the unchanged sibling.

  $ make_directory_targets_project 3.0
  $ echo left > left-input
  $ echo right > right-input
  $ cat > dune <<'EOF'
  > (rule
  >  (targets (dir left) (dir right))
  >  (deps left-input right-input)
  >  (action
  >   (bash "\| mkdir -p left/nested right/nested
  >         "\| cat left-input > left/nested/value.txt
  >         "\| cat right-input > right/nested/value.txt
  > )))
  > (rule
  >  (target left-summary)
  >  (deps (glob_files left/nested/*.txt))
  >  (action (with-stdout-to %{target} (run cat %{deps}))))
  > (rule
  >  (target right-summary)
  >  (deps (glob_files right/nested/*.txt))
  >  (action (with-stdout-to %{target} (run cat %{deps}))))
  > EOF
  $ start_dune --sandbox=copy --cache=disabled
  $ build left-summary right-summary
  Success
  $ cat _build/default/left-summary _build/default/right-summary
  left
  right

  $ echo changed-left > left-input
  $ build left-summary right-summary
  Success
  $ cat _build/default/left-summary _build/default/right-summary
  changed-left
  right

  $ echo changed-right > right-input
  $ build left-summary right-summary
  Success
  $ cat _build/default/left-summary _build/default/right-summary
  changed-left
  changed-right

A null build must not rerun either consumer. Each consumer should have run just
once initially and once when its own input changed.

  $ build left-summary right-summary
  Success
  $ stop_dune_quiet
  $ dune trace cat | jq_dune -s '
  >   [.[] | processes | .args.target_files[]?
  >    | select(endswith("-summary"))]
  >   | sort | group_by(.) | map({target: (.[0] | basename), runs: length})'
  [
    {
      "target": "left-summary",
      "runs": 2
    },
    {
      "target": "right-summary",
      "runs": 2
    }
  ]
