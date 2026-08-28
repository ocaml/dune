The current compatibility policy interprets factual target root kinds. Put the
recursive alias first, then an ordinary alias, and the file root last so
command-line order cannot explain the schedule.

  $ cat > dune-project <<'EOF'
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF
  $ cat > config <<'EOF'
  > (lang dune 2.0)
  > (sandboxing_preference none)
  > EOF
  $ mkdir bulk
  $ cat > dune <<'EOF'
  > (rule
  >  (targets shared-a shared-b)
  >  (action
  >   (progn
  >    (bash "printf 'shared-multi\\n' >> order; sleep 0.1")
  >    (write-file shared-a shared)
  >    (write-file shared-b shared))))
  > 
  > (rule
  >  (target cpp-1)
  >  (action
  >   (progn
  >    (bash "printf 'direct-cpp-1\\n' >> order; sleep 0.1")
  >    (write-file cpp-1 cpp))))
  > 
  > (rule
  >  (target cpp-2)
  >  (action
  >   (progn
  >    (bash "printf 'direct-cpp-2\\n' >> order; sleep 0.1")
  >    (write-file cpp-2 cpp))))
  > 
  > (rule
  >  (target barrier)
  >  (deps cpp-1 cpp-2 shared-a)
  >  (action
  >   (progn
  >    (bash "printf 'direct-barrier\\n' >> order; sleep 0.1")
  >    (write-file barrier barrier))))
  > 
  > (rule
  >  (target dynamic-source)
  >  (action
  >   (progn
  >    (bash "printf 'direct-dynamic-source\\n' >> order; sleep 0.1")
  >    (write-file dynamic-source dynamic))))
  > 
  > (rule
  >  (alias direct-dynamic)
  >  (action (dynamic-run action_plugin_helper read-file dynamic-source)))
  > 
  > (rule
  >  (target send.vo)
  >  (deps barrier (alias direct-dynamic))
  >  (action
  >   (progn
  >    (bash "printf 'direct-send\\n' >> order; sleep 0.1")
  >    (write-file send.vo direct))))
  > 
  > (rule
  >  (target normal-job)
  >  (action
  >   (progn
  >    (bash "printf 'normal-job\\n' >> order; sleep 0.1")
  >    (write-file normal-job normal))))
  > 
  > (alias
  >  (name normal)
  >  (deps normal-job))
  > EOF
  $ cat > bulk/dune <<'EOF'
  > (rule
  >  (target hammer-1)
  >  (action
  >   (progn
  >    (bash "printf 'bulk-hammer-1\\n' >> ../order; sleep 0.1")
  >    (write-file hammer-1 bulk))))
  > 
  > (rule
  >  (target hammer-2)
  >  (action
  >   (progn
  >    (bash "printf 'bulk-hammer-2\\n' >> ../order; sleep 0.1")
  >    (write-file hammer-2 bulk))))
  > 
  > (rule
  >  (target hammer-3)
  >  (action
  >   (progn
  >    (bash "printf 'bulk-hammer-3\\n' >> ../order; sleep 0.1")
  >    (write-file hammer-3 bulk))))
  > 
  > (alias
  >  (name default)
  >  (deps ../shared-b hammer-1 hammer-2 hammer-3))
  > EOF

The shared multi-target action may start from the leading recursive-alias root
before the other roots are ready. Once file-root generated-source work is
ready, it runs without lower-priority selection; then the alias runs before the
remaining recursive-alias hammers. The action-plugin request discovers
[dynamic-source] only after [send.vo]'s nested alias action starts, inherits the
file root, and also runs ahead of the lower priorities.

  $ rm -rf _build
  $ DUNE_CONFIG__PRIORITY_SCHEDULING=enabled DUNE_TRACE=scheduler,process \
  >   dune build --config-file config -j1 bulk @@normal send.vo
  $ cat _build/default/order
  shared-multi
  direct-cpp-1
  direct-cpp-2
  direct-barrier
  direct-dynamic-source
  direct-send
  normal-job
  bulk-hammer-1
  bulk-hammer-2
  bulk-hammer-3

Ready and start events correlate by a stable attempt ID. At least one file-root
attempt (rank 3) waited, and every started event reports the handle's current
priority and queue length.

  $ dune trace cat --trace-file _build/trace.csexp | jq -s -e '
  > [ .[] | select(.cat == "scheduler" and .name == "job-slot") ] as $slots
  > | [ $slots[] | select(.args.phase == "start") | .args.priority ] as $starts
  > | [ $slots[] | select(.args.phase == "start") | .args.attempt_id ] as $start_ids
  > | ($starts | index(3)) as $first_direct
  > | ($starts | length - 1 - (reverse | index(3))) as $last_direct
  > | [ .[] | select(.cat == "process" and .name == "start"
  >                 and .args.job_slot_attempt_id != null)
  >          | .args.job_slot_attempt_id ] as $processes
  > | (($slots | group_by(.args.attempt_id)
  >              | all(length == 2
  >                    and .[0].args.phase == "ready"
  >                    and .[1].args.phase == "start"))
  >    and ($slots | any(.args.phase == "ready"
  >                      and .args.priority == 3
  >                      and .args.waiting > 0))
  >    and ($slots | all(.args.policy == "current"
  >                      and .args.priority >= 0
  >                      and .args.waiting >= 0))
  >    and ($slots | map(select(.args.priority == 3))
  >                | all(.args.memo_generation >= 0
  >                      and .args.memo_node_id >= 0
  >                      and (.args.memo_roots | any(.kind == "file"))))
  >    and ($slots | any(.args.priority == 2
  >                      and (.args.memo_roots | any(.kind == "alias"))))
  >    and ($slots | any(.args.priority == 1
  >                      and (.args.memo_roots | any(.kind == "recursive-alias"))))
  >    and (($processes | length) > 0
  >         and ($processes | all(. as $id | $start_ids | index($id) != null)))
  >    and ($starts | any(. == 1) and any(. == 2) and any(. == 3))
  >    and ($starts[$first_direct:$last_direct + 1] | all(. == 3))
  >    and ($starts[$last_direct + 1:] | index(2) < index(1)))'
  true

Disabling the experiment changes admission order, not the required action set.
The shared multi-target action still runs exactly once in each build.

  $ cp _build/default/order enabled-order
  $ rm -rf _build
  $ DUNE_CONFIG__PRIORITY_SCHEDULING=disabled \
  >   dune build --config-file config -j1 bulk @@normal send.vo
  $ sort enabled-order > enabled-sorted
  $ sort _build/default/order > disabled-sorted
  $ diff enabled-sorted disabled-sorted
  $ for log in enabled-order _build/default/order; do
  >   test "$(grep -c '^shared-multi$' "$log")" = 1
  > done

A generated directory is a concrete file root. When it expands into two build
contexts, both resolved requests retain the one original root ID while
executing as distinct Memo nodes.

  $ mkdir grouping
  $ cd grouping
  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.24)
  > (context default)
  > (context
  >  (default
  >   (name second)))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target (dir generated-dir))
  >  (action
  >   (bash "mkdir %{target}; echo generated > %{target}/value")))
  > EOF
  $ DUNE_CONFIG__PRIORITY_SCHEDULING=enabled DUNE_TRACE=scheduler \
  >   dune build --root . -j1 generated-dir
  $ dune trace cat --trace-file _build/trace.csexp | jq -s -e '
  > [ .[] | select(.cat == "scheduler" and .name == "job-slot"
  >                 and .args.phase == "start" and .args.priority == 3
  >                 and (.args.memo_roots | any(.kind == "file"))) ] as $direct
  > | [ $direct[].args.memo_roots[] | select(.kind == "file") | .id ] as $roots
  > | (($direct | length) == 2
  >    and ($direct | map(.args.memo_generation) | unique | length) == 1
  >    and ($direct | map(.args.memo_node_id) | unique | length) == 2
  >    and ($roots | unique | length) == 1)'
  true
  $ cd ..
