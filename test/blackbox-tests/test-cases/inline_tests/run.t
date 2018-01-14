  $ $JBUILDER runtest -j1 --root .
      ocamldep mylib_test_runner.depends.ocamldep-output
      ocamlopt .ppx/ppx_inline_test/ppx.exe
           ppx mylib.pp.ml
      ocamldep mylib.depends.ocamldep-output
        ocamlc mylib.{cmi,cmo,cmt}
        ocamlc mylib_test_runner.{cmi,cmo,cmt}
      ocamlopt mylib.{cmx,o}
      ocamlopt mylib_test_runner.{cmx,o}
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib_test_runner.exe
  mylib_test_runner alias runtest (exit 2)
  (cd _build/default && ./mylib_test_runner.exe inline-test-runner mylib)
  File "mylib.ml", line 1, characters 0-25: <<assert false>> threw "Assert_failure mylib.ml:1:13".
    Raised at file "mylib.ml", line 1, characters 13-25
    Called from file "runtime-lib/runtime.ml", line 321, characters 8-12
    Re-raised at file "runtime-lib/runtime.ml", line 324, characters 6-13
    Called from file "runtime-lib/runtime.ml", line 411, characters 52-60
  
  FAILED 1 / 1 tests
  [1]
