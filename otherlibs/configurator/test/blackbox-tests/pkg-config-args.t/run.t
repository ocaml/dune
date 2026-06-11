  $ unset PKG_CONFIG_ARGN
  $ unset PKG_CONFIG

These tests show that setting `PKG_CONFIG_ARGN` passes extra args to `pkg-config`

  $ dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --version
  -> process exited with code 0
  -> stdout:
   | 2.4.3
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --personality $TARGET --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --personality
   | $TARGET
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --personality $TARGET --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --personality
   | $TARGET
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --personality $TARGET --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --personality
   | $TARGET
   | --libs
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --personality $TARGET --variable=prefix dummy-pkg
  -> process exited with code 0
  -> stdout:
   | value-for-prefix

  $ dune clean
  $ PKG_CONFIG_ARGN="--static" dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --libs
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --variable=prefix dummy-pkg
  -> process exited with code 0
  -> stdout:
   | value-for-prefix

`--personality` was only added in pkgconf 1.5.0 (and is only safe to use
from 1.7.0), so it is omitted when pkgconf is older:

  $ dune clean
  $ FAKE_PKGCONF_VERSION=1.4.2 dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a'
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --version
  -> process exited with code 0
  -> stdout:
   | 1.4.2
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --libs
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --variable=prefix dummy-pkg
  -> process exited with code 0
  -> stdout:
   | value-for-prefix
