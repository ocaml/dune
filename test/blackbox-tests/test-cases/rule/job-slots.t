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
  >    | {target: (.target_files[0] | split("/") | last), job_slots}]
  >   | group_by(.target)[]
  >   | "\(.[0].target): \(map(.job_slots) | sort)"'
  default: [1]
  half: [1,2]
  run: [1,3]
  too-many: [1,4]

A change of the total number of job slots does not re-run the rule or the
job_slots command:

  $ dune build -j 8 ./half
  $ dune trace cat | jq -s '
  >   [.[] | select(.cat == "process" and .name == "start"
  >                 and .args.target_files != null)] | length'
  0
  $ cat _build/default/half
  2

A change of the job_slots command does not re-run the rule or the job_slots
command:

  $ cat >dune <<'EOF'
  > (rule
  >  (target half)
  >  (job_slots (bash "exit 1"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./half
  $ cat _build/default/half
  2

Changing the job_slots command to one that resolves a program should not add
dependencies to the rule or trigger a rebuild:

  $ cat >dune <<'EOF'
  > (rule
  >  (target half)
  >  (job_slots (run sh -c "echo 'job_slots ran' >&2; echo 1"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

CR: Expanding job_slots in the rule's action builder adds its inferred
dependencies to the rule digest, causing both the probe and the rule to re-run.

  $ dune build -j 4 ./half
  job_slots ran
  $ cat _build/default/half
  1

When the rule needs to run again, the probe uses the new job limit and any
updated job_slots command:

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

  $ cat >dune <<'EOF'
  > (rule
  >  (target refresh)
  >  (deps input)
  >  (job_slots (bash "echo 'refresh probe ran' >&2; echo 1"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF
  $ dune build -j 3 refresh
  $ cat _build/default/refresh
  3
  $ echo third > input
  $ dune build -j 3 refresh
  refresh probe ran
  $ cat _build/default/refresh
  1

Alias rules also ignore changes to job_slots until the rule needs to run again:

  $ echo first > alias-input
  $ cat >dune <<'EOF'
  > (rule
  >  (alias slot-alias)
  >  (deps alias-input)
  >  (job_slots (bash "echo 'alias probe ran' >&2; echo $DUNE_JOBS"))
  >  (action (bash "echo \"alias action: $DUNE_JOB_SLOTS\"")))
  > EOF
  $ dune build -j 2 @slot-alias
  alias probe ran
  alias action: 2

  $ cat >dune <<'EOF'
  > (rule
  >  (alias slot-alias)
  >  (deps alias-input)
  >  (job_slots (bash "exit 1"))
  >  (action (bash "echo \"alias action: $DUNE_JOB_SLOTS\"")))
  > EOF
  $ dune build -j 2 @slot-alias

  $ cat >dune <<'EOF'
  > (rule
  >  (alias slot-alias)
  >  (deps alias-input)
  >  (job_slots (run sh -c "echo 'alias probe ran' >&2; echo 1"))
  >  (action (bash "echo \"alias action: $DUNE_JOB_SLOTS\"")))
  > EOF

CR: Inferred dependencies from job_slots also invalidate cached alias rules.

  $ dune build -j 2 @slot-alias
  alias probe ran
  alias action: 1

  $ echo second > alias-input
  $ dune build -j 2 @slot-alias
  alias probe ran
  alias action: 1

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
  File "dune", lines 1-4, characters 0-111:
  1 | (rule
  2 |  (target bad)
  3 |  (job_slots (echo foo))
  4 |  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  Error: The job_slots command must print a positive integer, but it printed
  "foo".
  [1]

  $ write_slot_rule 0
  $ dune build -j 4 ./bad
  File "dune", lines 1-4, characters 0-109:
  1 | (rule
  2 |  (target bad)
  3 |  (job_slots (echo 0))
  4 |  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  Error: The job_slots command must print a positive integer, but it printed
  "0".
  [1]

  $ write_slot_rule -3
  $ dune build -j 4 ./bad
  File "dune", lines 1-4, characters 0-110:
  1 | (rule
  2 |  (target bad)
  3 |  (job_slots (echo -3))
  4 |  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
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

The slot calculation should have its own working directory without the
rule's inputs. Print its directory to stderr so stdout is still a slot count.

  $ touch rule-input
  $ cat >dune <<'EOF'
  > (rule
  >  (alias sandboxed)
  >  (deps rule-input (sandbox always))
  >  (job_slots
  >   (bash "echo \"job_slots: $(pwd)\" >&2; if test -f rule-input; then echo 'job_slots sees rule-input' >&2; fi; echo 1"))
  >  (action (bash "echo \"action: $(pwd)\"; test -f rule-input")))
  > EOF

CR: The probe runs in the regular build directory, where it can see the rule's
input even though the input is not needed for the slot calculation.

  $ dune build @sandboxed > sandboxed.log 2>&1
  $ censor < sandboxed.log
  job_slots: $PWD/_build/default
  job_slots sees rule-input
  action: $PWD/_build/.sandbox/$DIGEST/default
