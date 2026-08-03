  $ unset PKG_CONFIG_ARGN
  $ unset PKG_CONFIG

These tests show that setting `PKG_CONFIG_ARGN` passes extra args to `pkg-config`

  $ STATIC_TEST__=true dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --version
  -> process exited with code 0
  -> stdout:
   | 2.4.3
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

  $ dune clean
  $ STATIC_TEST__=true PKG_CONFIG_ARGN="--static --cflags" dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | --libs
   | dummy-pkg

  $ dune clean
  $ STATIC_TEST__=false dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
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

  $ dune clean
  $ STATIC_TEST__=false PKG_CONFIG_ARGN="--static --cflags" dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --cflags --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --cflags --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --cflags
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --cflags --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --cflags
   | --libs
   | dummy-pkg

  $ dune clean
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

  $ dune clean
  $ PKG_CONFIG_ARGN="--static --cflags" dune build 2>&1 | awk '/run:.*bin\/pkgconf/{a=1}/stderr/{a=0}a' | sed s/$(ocamlc -config | sed -n "/^target:/ {s/target: //; p; }")/\$TARGET/g
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --print-errors dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --cflags dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | --cflags
   | dummy-pkg
  run: $TESTCASE_ROOT/_build/default/.bin/pkgconf --static --cflags --libs dummy-pkg
  -> process exited with code 0
  -> stdout:
   | --static
   | --cflags
   | --libs
   | dummy-pkg
