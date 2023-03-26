First, build silently to avoid some noise

  $ dune build

See that `test1/runtest`, which uses `fake_backend_1, only runs one inline test runner

  $ dune build --display short @test1/runtest
        ocamlc test1/.test_lib1.inline-tests/.test_lib1.inline-tests.eobjs/byte/dune__exe__Inline_test_runner_test_lib1.{cmi,cmo,cmt}
      ocamlopt test1/.test_lib1.inline-tests/.test_lib1.inline-tests.eobjs/native/dune__exe__Inline_test_runner_test_lib1.{cmx,o}
      ocamlopt test1/.test_lib1.inline-tests/inline_test_runner_test_lib1.exe
  inline_test_runner_test_lib1 alias test1/runtest

See that `test2/runtest`, which uses `fake_backend_2`, runs one inline test runner per partition

  $ dune build --display short @test2/runtest
        ocamlc test2/.test_lib2.inline-tests/.test_lib2.inline-tests.eobjs/byte/dune__exe__Inline_test_runner_test_lib2.{cmi,cmo,cmt}
      ocamlopt test2/.test_lib2.inline-tests/.test_lib2.inline-tests.eobjs/native/dune__exe__Inline_test_runner_test_lib2.{cmx,o}
      ocamlopt test2/.test_lib2.inline-tests/inline_test_runner_test_lib2.exe
  inline_test_runner_test_lib2 test2/.test_lib2.inline-tests/partitions-best
  inline_test_runner_test_lib2 alias test2/runtest
  inline_test_runner_test_lib2 alias test2/runtest
  inline_test_runner_test_lib2 alias test2/runtest

See that we indeed have 3 partitions

  $ cat _build/default/test2/.test_lib2.inline-tests/partitions-best
  p1
  p2
  p3
