  $ unset PKG_CONFIG_ARGN
  $ unset PKG_CONFIG

These tests show that setting `PKG_CONFIG_ARGN` passes extra args to `pkg-config`

  $ dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --personality $TARGET --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --personality
   | $TARGET
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --personality $TARGET --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --personality
   | $TARGET
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --personality $TARGET --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --personality
   | $TARGET
   | --libs
   | dummy-pkg
