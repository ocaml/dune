A rule can reserve more than one job slot with the (job_slots ...) field. Dune
runs the command in the field with the total number of job slots in DUNE_JOBS.
The command prints the number of slots that the rule reserves. The action sees
this number in DUNE_JOB_SLOTS.

The field is available since dune 3.25:

  $ make_dune_project 3.24

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
  Error: 'job_slots' is only available since version 3.25 of the dune language.
  Please update your dune-project file to have (lang dune 3.25).
  [1]

  $ make_dune_project 3.25

A rule without the field uses one slot. Dune limits the number that the command
prints to the range 1 to the total number of job slots:

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
  > (rule
  >  (target too-few)
  >  (job_slots (bash "echo 0"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./default ./half ./run ./too-many ./too-few
  $ cat _build/default/default _build/default/half _build/default/run \
  >   _build/default/too-many _build/default/too-few
  1
  2
  3
  4
  1

A change of the total number of job slots does not cause a rebuild:

  $ dune build -j 8 ./half
  $ cat _build/default/half
  2

A change of the job_slots command does not cause a rebuild:

  $ cat >dune <<'EOF'
  > (rule
  >  (target half)
  >  (job_slots (bash "echo 1"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./half
  $ cat _build/default/half
  2

The command must print an integer:

  $ cat >dune <<'EOF'
  > (rule
  >  (target bad)
  >  (job_slots (bash "echo foo"))
  >  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  > EOF

  $ dune build -j 4 ./bad
  File "dune", lines 1-4, characters 0-118:
  1 | (rule
  2 |  (target bad)
  3 |  (job_slots (bash "echo foo"))
  4 |  (action (with-stdout-to %{target} (bash "echo $DUNE_JOB_SLOTS"))))
  Error: The job_slots command must print an integer, but it printed "foo".
  [1]

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

A rule that reserves all of the job slots does not run at the same time as
another rule:

  $ cat >dune <<'EOF'
  > (rule
  >  (alias exclusive)
  >  (job_slots (bash "echo $DUNE_JOBS"))
  >  (action
  >   (bash
  >    "if mkdir $SYNC/busy 2>/dev/null; then sleep 1; rmdir $SYNC/busy; else echo x overlaps; fi")))
  > (rule
  >  (alias exclusive)
  >  (action
  >   (bash
  >    "if mkdir $SYNC/busy 2>/dev/null; then sleep 1; rmdir $SYNC/busy; else echo y overlaps; fi")))
  > EOF

  $ dune build -j 2 @exclusive
