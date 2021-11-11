Shows what happens when Dune tries to kill an action that has sub-processes.

  $ . ../watching/helpers.sh
  $ export PATH=$PWD/bin:$PATH

  $ mkdir test; cd test

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune <<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "sub_process.exe > ../../../output")))
  > EOF

  $ export BEACON_FILE=$PWD/test-started

  $ start_dune
  $ build . > /dev/null 2>&1 &

sub_process.exe spawns a sub-process that creates $BEACON_FILE. We
wait for the beacon to be notified that the sub-process has started:

  $ with_timeout dune_cmd wait-for-file-to-appear $BEACON_FILE
  $ CHILD_PID=`cat $BEACON_FILE`

  $ cat ../output
  Creating $TESTCASE_ROOT/test/test-started...
  Done.

  $ find
  .
  ./_build
  ./_build/default
  ./_build/default/.dune
  ./_build/default/.dune/configurator.v2
  ./_build/default/.dune/configurator
  ./_build/.actions
  ./_build/.actions/default
  ./_build/log
  ./_build/.filesystem-clock
  ./_build/.rpc
  ./_build/.rpc/dune
  ./_build/.sync
  ./test-started
  ./dune-output
  ./dune
  ./dune-project

Now we stop Dune, which should normally kill all sub-processes:

  $ stop_dune

  $ if kill -s 0 $CHILD_PID 2> /dev/null; then
  >   echo "FAILURE: child is still running"
  > else
  >   echo "SUCCESS: child has exited"
  > fi
  SUCCESS: child has exited
