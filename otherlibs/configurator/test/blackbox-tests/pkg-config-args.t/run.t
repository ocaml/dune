These tests show that setting `PKG_CONFIG_ARGN` passes extra args to `pkg-config`

  $ dune build 2>&1 | awk '/run:.*bin\/pkg-config/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --libs
   | dummy-pkg

  $ dune clean
  $ PKG_CONFIG_ARGN="--static" dune build 2>&1 | awk '/run:.*bin\/pkg-config/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --static --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static--cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkg-config --static --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static--libs
   | dummy-pkg
