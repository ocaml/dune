Shows what happens when Dune tries to kill an action that has sub-processes.

  $ export DUNE_DEBUG_BLAH=1

  $ . ../watching/helpers.sh
  $ export PATH=$PWD/bin:$PATH

  $ mkdir test; cd test

  $ echo '(lang dune 3.0)' > dune-project
  $ cat >dune <<"EOF"
  > (rule
  >  (alias default)
  >  (action (system "sub_process.exe ../../../output")))
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

  $ find . | sort
  .
  ./_build
  ./_build/.actions
  ./_build/.actions/default
  ./_build/.filesystem-clock
  ./_build/.rpc
  ./_build/.rpc/dune
  ./_build/.sync
  ./_build/default
  ./_build/default/.dune
  ./_build/default/.dune/configurator
  ./_build/default/.dune/configurator.v2
  ./_build/log
  ./dune
  ./dune-output
  ./dune-project
  ./test-started

Now we stop Dune, which should normally kill all sub-processes:

  $ stop_dune
  XXX: watching "_build/.sync"
  XXX: watching "dune-workspace"
  XXX: watching "."
  XXX: watching "dune-workspace"
  XXX: raw inotify event: watch=1 cookie=0 events=CREATE "0"
  XXX: raw inotify event: watch=1 cookie=0 events=CLOSE_WRITE "0"
  XXX: inotify event: created _build/.sync/0
  XXX: inotify event: modified _build/.sync/0
  XXX: raw inotify event: watch=1 cookie=0 events=DELETE "0"
  XXX: inotify event: unlinked _build/.sync/0
  XXX: received sync: 0
  XXX: watching "."
  XXX: watching "dune"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocamlc.opt"
  XXX: watching
  "$TESTCASE_ROOT/bin"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocamlc.opt"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocamlc"
  XXX: watching
  "$TESTCASE_ROOT/bin"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocamlc"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocamlc"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc.opt"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc.opt"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/install/default/bin/ocamlc"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlc.opt"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlopt.opt"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocaml"
  XXX: watching
  "$TESTCASE_ROOT/bin"
  XXX: watching
  "$TESTCASE_ROOT/bin/ocaml"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocaml"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin"
  XXX: watching
  "/usr/local/home/jdimino/dune/_build/default/test/blackbox-tests/test-cases/.bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin"
  XXX: watching "/usr/local/home/jdimino/dune/_build/install/default/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocaml"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamldep.opt"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlmklib.opt"
  XXX: watching "/usr/local/home/jdimino/opam-root/4.12.1/bin/ocamlobjinfo.opt"
  XXX: raw inotify event: watch=2 cookie=0 events=CREATE "test-started"
  XXX: raw inotify event: watch=2 cookie=0 events=CLOSE_WRITE "test-started"
  XXX: inotify event: created ./test-started
  XXX: inotify event: modified ./test-started
  XXX: got fs event: { path = In_source_tree "test-started"; kind = "Created" }
  XXX: got fs event:
  { path = In_source_tree "test-started"; kind = "File_changed" }
  XXX: raw inotify event: watch=1 cookie=0 events=CREATE "1"
  XXX: raw inotify event: watch=1 cookie=0 events=CLOSE_WRITE "1"
  XXX: inotify event: created _build/.sync/1
  XXX: inotify event: modified _build/.sync/1
  XXX: received sync: 1
  XXX: watching "."
  XXX: raw inotify event: watch=1 cookie=0 events=DELETE "1"
  XXX: inotify event: unlinked _build/.sync/1
  XXX: raw inotify event: watch=2 cookie=0 events=CLOSE_WRITE "test-started"
  XXX: inotify event: modified ./test-started
  XXX: got fs event:
  { path = In_source_tree "test-started"; kind = "File_changed" }

  $ if kill -s 0 $CHILD_PID 2> /dev/null; then
  >   echo "FAILURE: child is still running"
  > else
  >   echo "SUCCESS: child has exited"
  > fi
  SUCCESS: child has exited
