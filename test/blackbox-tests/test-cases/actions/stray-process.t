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

  $ with_timeout dune_cmd wait-for-file-to-appear $BEACON_FILE

  $ stop_dune
  waiting for inotify sync
  waited for inotify sync

Both processes should have terminated gracefully as sub_process.exe
correctly handles SIGTERM:

  $ if test -f $BEACON_FILE.parent; then echo "ok"; else echo "fail"; fi
  fail
  $ if test -f $BEACON_FILE.child; then echo "ok"; else echo "fail"; fi
  fail

The sub-process is still running, which is bad:

  $ rm -f $BEACON_FILE
  $ with_timeout dune_cmd wait-for-file-to-appear $BEACON_FILE

