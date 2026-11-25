A rule can reserve more than one job slot with the (job_slots ...) field. Dune
runs the command in the field with the total number of job slots in DUNE_JOBS.
The command prints the number of slots that the rule reserves. The action sees
this number in DUNE_JOB_SLOTS.

The field is available since dune 3.26:

  $ make_dune_project 3.25

  $ cat >dune <<'EOF'
  > (rule
  >  (target half)
  >  (job_slots (bash "echo $((DUNE_JOBS / 2))"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build ./half
  File "dune", line 3, characters 1-45:
  3 |  (job_slots (bash "echo $((DUNE_JOBS / 2))"))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'job_slots' is only available since version 3.26 of the dune language.
  Please update your dune-project file to have (lang dune 3.26).
  [1]

  $ make_dune_project 3.26

A rule without the field uses one slot. Dune caps positive requests at the total
number of job slots:

  $ cat >dune <<'EOF'
  > (rule
  >  (target default)
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > (rule
  >  (target half)
  >  (job_slots (bash "echo $((DUNE_JOBS / 2))"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > (rule
  >  (target run)
  >  (job_slots (run echo 3))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > (rule
  >  (target too-many)
  >  (job_slots (bash "echo 100"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./default ./half ./run ./too-many
  $ cat _build/default/default _build/default/half _build/default/run \
  >   _build/default/too-many
  1
  2
  3
  4

Process start events report the granted slots, including the one slot used by
commands that compute job slots:

  $ dune trace cat | jq -sr '
  >   [.[] | select(.cat == "process" and .name == "start")
  >    | .args | select(.target_files != null)
  >    | {target: (.target_files[0] |
  >        if contains("/.actions/") then "job_slots probe"
  >        else split("/") | last end), job_slots}]
  >   | group_by(.target)[]
  >   | "\(.[0].target): \(map(.job_slots) | sort)"'
  default: [1]
  half: [2]
  job_slots probe: [1,1,1]
  run: [3]
  too-many: [4]

A change of the total number of job slots does not cause a rebuild:

  $ dune build -j 8 ./half
  $ cat _build/default/half
  2

A change of the job_slots command does not cause a rebuild:

  $ cat >dune <<'EOF'
  > (rule
  >  (target half)
  >  (job_slots (bash "exit 1"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./half
  $ cat _build/default/half
  2

When the rule needs to run again, the probe uses the new job limit.

  $ cat >dune <<'EOF'
  > (rule
  >  (target refresh)
  >  (deps input)
  >  (job_slots (bash "echo $DUNE_JOBS"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF
  $ echo first > input
  $ dune build -j 2 refresh
  $ cat _build/default/refresh
  2
  $ echo second > input
  $ dune build -j 3 refresh
  $ cat _build/default/refresh
  3

The command must print a positive integer. Non-integers, zero, and negative
values are rejected before the rule's action runs:

  $ write_slot_rule() {
  >   cat >dune <<EOF
  > (rule
  >  (target bad)
  >  (job_slots (echo $1))
  >  (action (with-stdout-to %{target} (bash "echo \$DUNE_JOB_SLOTS"))))
  > EOF
  > }

  $ write_slot_rule foo
  $ dune build -j 4 ./bad
  File "dune", line 3, characters 12-22:
  3 |  (job_slots (echo foo))
                  ^^^^^^^^^^
  Error: The job_slots command must print a positive integer, but it printed
  "foo".
  [1]

  $ write_slot_rule 0
  $ dune build -j 4 ./bad
  File "dune", line 3, characters 12-20:
  3 |  (job_slots (echo 0))
                  ^^^^^^^^
  Error: The job_slots command must print a positive integer, but it printed
  "0".
  [1]

  $ write_slot_rule -3
  $ dune build -j 4 ./bad
  File "dune", line 3, characters 12-21:
  3 |  (job_slots (echo -3))
                  ^^^^^^^^^
  Error: The job_slots command must print a positive integer, but it printed
  "-3".
  [1]
  $ test ! -e _build/default/bad

One slot is accepted:

  $ write_slot_rule 1
  $ dune build -j 4 ./bad
  $ cat _build/default/bad
  1

Two rules that each reserve half of the job slots run at the same time. Each
action waits until the other action starts. The actions share a directory
outside of the sandbox, so we disable landlock:

  $ export DUNE_CONFIG__LANDLOCK=disabled
  $ export SYNC=$PWD/sync
  $ mkdir sync

  $ cat >dune <<'EOF'
  > (rule
  >  (alias both)
  >  (job_slots (bash "echo $((DUNE_JOBS / 2))"))
  >  (action
  >   (bash
  >    "touch $SYNC/a; for i in $(seq 100); do if [ -e $SYNC/b ]; then echo a met b; exit 0; fi; sleep 0.1; done; echo a timeout")))
  > (rule
  >  (alias both)
  >  (job_slots (bash "echo $((DUNE_JOBS / 2))"))
  >  (action
  >   (bash
  >    "touch $SYNC/b; for i in $(seq 100); do if [ -e $SYNC/a ]; then echo b met a; exit 0; fi; sleep 0.1; done; echo b timeout")))
  > EOF

  $ dune build -j 4 @both 2>&1 | sort
  a met b
  b met a

A job can start with its full weight as long as at least one slot is free.
The light job holds one slot while waiting for the heavy job to start. The
heavy job requests two slots, and its slot calculation waits for the light
job to start first. Requesting all slots does not guarantee exclusivity:

  $ cat >dune <<'EOF'
  > (rule
  >  (alias overcommit)
  >  (job_slots
  >   (bash
  >    "for i in $(seq 100); do
  >       if [ -e $SYNC/light ]; then echo $DUNE_JOBS; exit 0; fi
  >       sleep 0.1
  >     done
  >     echo light did not start >&2; exit 1"))
  >  (action (bash "touch $SYNC/heavy; echo heavy: $DUNE_JOB_SLOTS")))
  > (rule
  >  (alias overcommit)
  >  (action
  >   (bash
  >    "touch $SYNC/light
  >     for i in $(seq 100); do
  >       if [ -e $SYNC/heavy ]; then echo light: $DUNE_JOB_SLOTS; exit 0; fi
  >       sleep 0.1
  >     done
  >     echo light timeout")))
  > EOF

  $ dune build -j 2 @overcommit 2>&1 | sort
  heavy: 2
  light: 1

The slot calculation should have its own sandbox without the rule's inputs,
even when the sandboxing preference is `none`. Print its directory to stderr
so stdout is still a slot count.

  $ touch rule-input
  $ cat >dune <<'EOF'
  > (rule
  >  (alias sandboxed)
  >  (deps rule-input (sandbox always))
  >  (job_slots
  >   (bash "echo \"job_slots: $(pwd)\" >&2; if test -f rule-input; then echo 'job_slots sees rule-input' >&2; fi; echo 1"))
  >  (action (bash "echo \"action: $(pwd)\"; test -f rule-input")))
  > EOF

  $ dune build --sandbox=none @sandboxed > sandboxed.log 2>&1
  $ censor < sandboxed.log
  job_slots: $PWD/_build/.sandbox/$DIGEST1/default
  action: $PWD/_build/.sandbox/$DIGEST2/default
