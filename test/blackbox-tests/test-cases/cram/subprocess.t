Testing the termination of subprocesses in cram tests. We first create a dune
project with a single cram test.

  $ make_dune_project 3.19

Cleanup via process groups is only enabled on Linux when pid_max is high enough
that pid/pgid reuse is unlikely during the grace period.

  $ pgid_cleanup_enabled () {
  >   [ "$(uname -s)" = "Linux" ] || return 1
  >   [ -r /proc/sys/kernel/pid_max ] || return 1
  >   pid_max=$(cat /proc/sys/kernel/pid_max)
  >   case "$pid_max" in
  >     ''|*[!0-9]*) return 1 ;;
  >   esac
  >   [ "$pid_max" -ge 4000000 ]
  > }

We create a file for tracking the PID of a subprocess.

  $ pidFile="$PWD/pid.txt"

  $ assert_process_terminated () {
  >   pid_file="$1"
  >   [ -s "$pid_file" ] || {
  >     echo "pid file was not created"
  >     return 1
  >   }
  >   if ps -p "$(cat "$pid_file")" > /dev/null
  >   then
  >     echo "Process still running"
  >     return 1
  >   fi
  > }

We create a cram test that spawns a subprocess and records its PID in the file
we gave before.

  $ cat > mycram.t <<EOF
  >   $ sleep 5 &
  >   $ echo \$! > $pidFile
  > EOF

We can now run this test, which will record its PID in the file. If pgid cleanup
is enabled, the subprocess should be terminated before the test action finishes.

  $ if pgid_cleanup_enabled
  > then
  >   dune runtest mycram.t
  >   assert_process_terminated "$pidFile"
  > fi

Processes that ignore SIGTERM are escalated to SIGKILL before the test action
finishes.

  $ stubbornPidFile="$PWD/stubborn-pid.txt"
  $ cat > stubborn.t <<EOF
  >   $ (trap '' TERM; sleep 5) &
  >   $ echo \$! > $stubbornPidFile
  > EOF

  $ if pgid_cleanup_enabled
  > then
  >   dune runtest stubborn.t
  >   assert_process_terminated "$stubbornPidFile"
  > fi
