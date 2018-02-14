normal inline tests
  $ $JBUILDER build @ppx_inline_test/runtest -j1 --display short --root .
      ocamldep ppx_inline_test/mylib_test_runner.ml-gen.d
        ocamlc .mylib.objs/mylib.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/mylib.{cmx,o}
        ocamlc ppx_inline_test/.mylib_test_runner.eobjs/mylib_test_runner.{cmi,cmo,cmt}
      ocamlopt mylib.{a,cmxa}
      ocamlopt ppx_inline_test/.mylib_test_runner.eobjs/mylib_test_runner.{cmx,o}
      ocamlopt ppx_inline_test/mylib_test_runner.exe
  mylib_test_runner alias ppx_inline_test/runtest

ppx_expect tests
  $ $JBUILDER build @ppx_expect/runtest -j1 --display short --root . 2>&1 | sed 's|/[^"]*/lib_with_expect_tests.ml|SRC/lib_with_expect_tests.ml|'
      ocamldep ppx_expect/lib_with_expect_tests_test_runner.ml-gen.d
      ocamlopt .ppx/ppx_expect/ppx.exe
           ppx ppx_expect/lib_with_expect_tests.pp.ml
      ocamldep ppx_expect/lib_with_expect_tests.pp.ml.d
        ocamlc ppx_expect/.lib_with_expect_tests.objs/lib_with_expect_tests.{cmi,cmo,cmt}
        ocamlc ppx_expect/.lib_with_expect_tests_test_runner.eobjs/lib_with_expect_tests_test_runner.{cmi,cmo,cmt}
      ocamlopt ppx_expect/.lib_with_expect_tests.objs/lib_with_expect_tests.{cmx,o}
      ocamlopt ppx_expect/.lib_with_expect_tests_test_runner.eobjs/lib_with_expect_tests_test_runner.{cmx,o}
      ocamlopt ppx_expect/lib_with_expect_tests.{a,cmxa}
      ocamlopt ppx_expect/lib_with_expect_tests_test_runner.exe
  lib_with_expect_tests_test_runner alias ppx_expect/runtest (exit 2)
  (cd _build/default/ppx_expect && ./lib_with_expect_tests_test_runner.exe inline-test-runner lib_with_expect_tests)
  Fatal error: exception (Sys_error
    "SRC/lib_with_expect_tests.ml: No such file or directory")
  Raised by primitive operation at file "pervasives.ml", line 385, characters 28-54
  Called from file "src/in_channel.ml", line 20, characters 45-66
  Called from file "src/in_channel.ml" (inlined), line 90, characters 21-49
  Called from file "evaluator/ppx_expect_evaluator.ml", line 50, characters 4-83
  Called from file "src/list.ml", line 318, characters 13-17
  Called from file "evaluator/ppx_expect_evaluator.ml", line 117, characters 2-62
  Called from file "list.ml", line 67, characters 20-23
  Called from file "list.ml", line 67, characters 32-39
  Called from file "runtime-lib/runtime.ml", line 590, characters 2-49
  Called from file "ppx_expect/lib_with_expect_tests_test_runner.ml-gen", line 1, characters 9-44

composite ppx
  $ $JBUILDER build @composite-ppx/runtest -j1 --display short --root .
      ocamldep composite-ppx/lib_tested_test_runner.ml-gen.d
        ocamlc composite-ppx/.ppx_has_inline_tests.objs/ppx_has_inline_tests.{cmi,cmo,cmt}
      ocamlopt composite-ppx/.ppx_has_inline_tests.objs/ppx_has_inline_tests.{cmx,o}
      ocamlopt composite-ppx/ppx_has_inline_tests.{a,cmxa}
      ocamlopt .ppx/ppx_has_inline_tests@/ppx.exe
           ppx composite-ppx/lib_tested.pp.ml
      ocamldep composite-ppx/lib_tested.pp.ml.d
        ocamlc composite-ppx/.lib_tested.objs/lib_tested.{cmi,cmo,cmt}
        ocamlc composite-ppx/.lib_tested_test_runner.eobjs/lib_tested_test_runner.{cmi,cmo,cmt}
      ocamlopt composite-ppx/.lib_tested.objs/lib_tested.{cmx,o}
      ocamlopt composite-ppx/.lib_tested_test_runner.eobjs/lib_tested_test_runner.{cmx,o}
      ocamlopt composite-ppx/lib_tested.{a,cmxa}
      ocamlopt composite-ppx/lib_tested_test_runner.exe
  lib_tested_test_runner alias composite-ppx/runtest (exit 2)
  (cd _build/default/composite-ppx && ./lib_tested_test_runner.exe inline-test-runner lib_tested)
  File "composite-ppx/lib_tested.ml", line 1, characters 0-25: <<assert false>> threw "Assert_failure composite-ppx/lib_tested.ml:1:13".
    Raised at file "composite-ppx/lib_tested.ml", line 1, characters 13-25
    Called from file "runtime-lib/runtime.ml", line 321, characters 8-12
    Re-raised at file "runtime-lib/runtime.ml", line 324, characters 6-13
    Called from file "runtime-lib/runtime.ml", line 411, characters 52-60
  
  FAILED 1 / 1 tests
  [1]
