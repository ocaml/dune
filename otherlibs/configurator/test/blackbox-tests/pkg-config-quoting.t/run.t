These tests show how various pkg-config invocations get quotes:
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
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --print-errors 'gtk+-quartz-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | gtk+-quartz-3.0 >= 3.18
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --cflags 'gtk+-quartz-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | --cflags
   | gtk+-quartz-3.0 >= 3.18
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --libs 'gtk+-quartz-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | --libs
   | gtk+-quartz-3.0 >= 3.18
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --print-errors 'gtksourceview-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | gtksourceview-3.0 >= 3.18
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --cflags 'gtksourceview-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | --cflags
   | gtksourceview-3.0 >= 3.18
  run: $TESTCASE_ROOT/_build/install/default/bin/pkg-config --libs 'gtksourceview-3.0 >= 3.18'
  -> process exited with code 0
  -> stdout:
   | --libs
   | gtksourceview-3.0 >= 3.18
