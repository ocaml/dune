These tests show that setting `PKG_CONFIG_ARGN` passes extra args to `pkg-config`

  $ dune build 2>&1 | awk '/run:.*bin\/pkg-config/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --print-errors gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | gtk+-quartz-3.0
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --cflags gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | --cflags
   | gtk+-quartz-3.0
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --libs gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | --libs
   | gtk+-quartz-3.0

  $ dune clean
  $ PKG_CONFIG_ARGN="--static" dune build 2>&1 | awk '/run:.*bin\/pkg-config/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --print-errors gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | gtk+-quartz-3.0
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --cflags gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | --cflags
   | gtk+-quartz-3.0
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --libs gtk+-quartz-3.0
  -> process exited with code 0
  -> stdout:
   | --libs
   | gtk+-quartz-3.0
