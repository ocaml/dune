Dune is a Linux subreaper, so a process that leaves the action's process group
is adopted by Dune and cleaned up after the build.

  $ make_dune_project 3.23

  $ pid_file=$TMPDIR/escaped-pid
  $ cat >dune <<EOF
  > (rule
  >  (target first)
  >  (action
  >   (progn
  >    (run %{exe:bin/sub_process.exe} setsid "$pid_file")
  >    (with-stdout-to first (echo done)))))
  > EOF

  $ start_dune

  $ build first
  Success

The process called setsid, so it left the process group that Dune created for
the action. Dune scans its children after the build and terminates adopted
processes.

  $ with_timeout dune_cmd wait-for-file-to-appear "$pid_file"
  $ child_pid=$(cat "$pid_file")
  $ if kill -0 "$child_pid" 2>/dev/null; then
  >   echo "FAILURE: escaped process is still running"
  > else
  >   echo "SUCCESS: escaped process was killed"
  > fi
  SUCCESS: escaped process was killed

  $ stop_dune_quiet

  $ dune trace cat | jq -r '
  >   select(.name == "child-process-cleanup" and .args.pid_count > 0)
  > | .args.stage
  > ' | sort -u
  finished
  sent-signal
  started

If a file change cancels a build, Dune runs subreaper cleanup before starting
the replacement build.

  $ cancel_pid_file=$TMPDIR/cancel-pid
  $ cancel_started_file=$TMPDIR/cancel-started
  $ cancel_release_file=$TMPDIR/cancel-release
  $ echo initial > watched-input
  $ cat >>dune <<EOF
  > (rule
  >  (target eighth)
  >  (deps watched-input)
  >  (action
  >   (bash "\| if grep -q initial watched-input; then
  >         "\|   %{exe:bin/sub_process.exe} setsid '$cancel_pid_file'
  >         "\| fi
  >         "\| touch '$cancel_started_file'
  >         "\| while [ ! -e '$cancel_release_file' ]; do
  >         "\|   sleep 0.01
  >         "\| done
  >         "\| echo done > eighth
  > )))
  > EOF

  $ start_dune

  $ dune rpc build --wait eighth >cancel-build.out 2>&1 &
  $ cancel_build=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$cancel_pid_file"
  $ with_timeout dune_cmd wait-for-file-to-appear "$cancel_started_file"
  $ cancel_child_pid=$(cat "$cancel_pid_file")
  $ echo changed > watched-input
  $ if wait_for_pid_to_exit_with_timeout "$cancel_child_pid" 200; then
  >   echo "SUCCESS: escaped process was killed before the replacement build"
  > else
  >   echo "FAILURE: escaped process survived into the replacement build"
  > fi
  SUCCESS: escaped process was killed before the replacement build

  $ touch "$cancel_release_file"
  $ wait_for_pid_to_exit_with_timeout "$cancel_build" 200 || (cat cancel-build.out; false)
  $ wait "$cancel_build"
  $ cat cancel-build.out
  Success

  $ stop_dune_quiet

If a child exits during the SIGTERM grace period and leaves another child
behind, the newly adopted child gets SIGTERM before cleanup escalates to
SIGKILL.

  $ parent_pid_file=$TMPDIR/term-parent-pid
  $ child_cleanup_file=$TMPDIR/term-child-cleanup
  $ cat >>dune <<EOF
  > (rule
  >  (target fourth)
  >  (action
  >   (progn
  >    (run %{exe:bin/sub_process.exe} term-child "$parent_pid_file" "$child_cleanup_file")
  >    (with-stdout-to fourth (echo done)))))
  > (rule
  >  (target fifth)
  >  (action (with-stdout-to fifth (echo done))))
  > EOF

  $ start_dune

  $ build fourth
  Success

  $ with_timeout dune_cmd wait-for-file-to-appear "$parent_pid_file"

  $ build fifth
  Success

  $ with_timeout dune_cmd wait-for-file-to-appear "$child_cleanup_file"

  $ stop_dune_quiet

Shutdown also runs subreaper cleanup, so an escaped child from the final build
is terminated before Dune exits.

  $ shutdown_pid_file=$TMPDIR/shutdown-pid
  $ cat >>dune <<EOF
  > (rule
  >  (target sixth)
  >  (action
  >   (progn
  >    (run %{exe:bin/sub_process.exe} setsid "$shutdown_pid_file")
  >    (with-stdout-to sixth (echo done)))))
  > EOF

  $ dune build sixth

  $ with_timeout dune_cmd wait-for-file-to-appear "$shutdown_pid_file"
  $ shutdown_child_pid=$(cat "$shutdown_pid_file")

  $ if kill -0 "$shutdown_child_pid" 2>/dev/null; then
  >   kill "$shutdown_child_pid" 2>/dev/null || true
  >   wait_for_pid_to_exit_with_timeout "$shutdown_child_pid" 200 || true
  >   echo "FAILURE: escaped process survived shutdown"
  > else
  >   echo "SUCCESS: escaped process was cleaned up during shutdown"
  > fi
  SUCCESS: escaped process was cleaned up during shutdown
