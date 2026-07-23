Dune is a Linux subreaper, so a process that leaves the action's process group
is adopted by Dune and cleaned up after the build.

  $ make_dune_project 3.23

  $ wait_for_child_process_cleanup_finished () {
  >   child_pid="$1"
  >   wait_for_trace_jq_true '
  >     any(.[];
  >       .name == "child-process-cleanup"
  >       and .args.stage == "finished"
  >       and ((.args.pids // []) | index('"$child_pid"')))
  >   '
  > }

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
  $ wait_for_child_process_cleanup_finished "$child_pid"
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
Action runner workers are Dune-owned child processes, not escaped action
children. Subreaper cleanup must preserve workers but still clean escaped
children adopted by the worker at the end of a build.

  $ action_runner_pid_file=$TMPDIR/action-runner-pid
  $ action_runner_wait_started=$TMPDIR/action-runner-wait-started
  $ action_runner_release=$TMPDIR/action-runner-release
  $ action_runner_cancel_pid_file=$TMPDIR/action-runner-cancel-pid
  $ action_runner_cancel_started=$TMPDIR/action-runner-cancel-started
  $ cat >>dune <<EOF
  > (rule
  >  (target third)
  >  (action
  >   (progn
  >    (run %{exe:bin/sub_process.exe} setsid "$action_runner_pid_file")
  >    (bash "touch '$action_runner_wait_started'; while [ ! -e '$action_runner_release' ]; do sleep 0.01; done")
  >    (with-stdout-to third (echo done)))))
  > (rule
  >  (target seventh)
  >  (action
  >   (progn
  >    (run %{exe:bin/sub_process.exe} setsid "$action_runner_cancel_pid_file")
  >    (bash "touch '$action_runner_cancel_started'; while true; do sleep 1; done")
  >    (with-stdout-to seventh (echo done)))))
  > EOF

  $ start_dune --action-runner

  $ dune rpc build --wait third >action-runner-build.out 2>&1 &
  $ action_runner_build=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$action_runner_pid_file"
  $ with_timeout dune_cmd wait-for-file-to-appear "$action_runner_wait_started"
  $ action_runner_child_pid=$(cat "$action_runner_pid_file")
  $ if kill -0 "$action_runner_child_pid" 2>/dev/null; then
  >   echo "SUCCESS: escaped action-runner process is still running during the build"
  > else
  >   echo "FAILURE: escaped action-runner process was killed before the build ended"
  > fi
  SUCCESS: escaped action-runner process is still running during the build

  $ touch "$action_runner_release"
  $ wait_for_pid_to_exit_with_timeout "$action_runner_build" 200 || (cat action-runner-build.out; false)
  $ wait "$action_runner_build"
  $ cat action-runner-build.out
  Success

  $ if kill -0 "$action_runner_child_pid" 2>/dev/null; then
  >   kill "$action_runner_child_pid" 2>/dev/null || true
  >   wait_for_pid_to_exit_with_timeout "$action_runner_child_pid" 200 || true
  >   echo "FAILURE: escaped action-runner process is still running after the build"
  > else
  >   echo "SUCCESS: escaped action-runner process was killed after the build"
  > fi
  SUCCESS: escaped action-runner process was killed after the build

Action-runner cancellation also cleans escaped children.

This test observes eventual cleanup after cancelling the RPC client. We would
like to test the stronger property that cleanup has completed before the action
runner's cancel-build response returns.

  $ dune rpc build --wait seventh >action-runner-cancel.out 2>&1 &
  $ action_runner_cancel_build=$!
  $ with_timeout dune_cmd wait-for-file-to-appear "$action_runner_cancel_pid_file"
  $ with_timeout dune_cmd wait-for-file-to-appear "$action_runner_cancel_started"
  $ action_runner_cancel_child_pid=$(cat "$action_runner_cancel_pid_file")
  $ if kill -0 "$action_runner_cancel_child_pid" 2>/dev/null; then
  >   echo "SUCCESS: escaped action-runner process is still running before cancellation"
  > else
  >   echo "FAILURE: escaped action-runner process was not running before cancellation"
  > fi
  SUCCESS: escaped action-runner process is still running before cancellation

  $ kill "$action_runner_cancel_build"
  $ wait "$action_runner_cancel_build" || true
  $ if wait_for_pid_to_exit_with_timeout "$action_runner_cancel_child_pid" 200; then
  >   echo "SUCCESS: escaped action-runner process was killed after cancellation"
  > else
  >   kill "$action_runner_cancel_child_pid" 2>/dev/null || true
  >   wait_for_pid_to_exit_with_timeout "$action_runner_cancel_child_pid" 200 || true
  >   echo "FAILURE: escaped action-runner process is still running after cancellation"
  > fi
  SUCCESS: escaped action-runner process was killed after cancellation

  $ stop_dune_quiet
