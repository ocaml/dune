Shows what happens when Dune tries to kill an action that has sub-processes.

  $ . ../watching/helpers.sh
  $ export PATH=$PWD/bin:$PATH

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune <<"EOF"
  > (rule
  >  (action
  >   (progn
  >    (run sub_process.exe)
  >    (with-stdout-to x (echo "")))))
  > EOF

  $ export BEACON_FILE=$PWD/test-started

  $ start_dune
  $ build x >/dev/null 2>&1 &

sub_process.exe spawns a sub-process that creates $BEACON_FILE. We
wait for the beacon to be notified that the sub-process has started:

  $ with_timeout dune_cmd wait-for-file-to-appear $BEACON_FILE
  $ CHILD_PID=`cat $BEACON_FILE`

Now we stop Dune, which should normally kill all sub-processes:

  $ stop_dune

  $ if kill -s 0 $CHILD_PID 2> /dev/null; then
  >   echo "FAILURE: child is still running"
  > else
  >   echo "SUCCESS: child has exited"
  > fi
  SUCCESS: child has exited
