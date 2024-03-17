First, build silently to avoid some noise

  $ dune build

See that `test1/runtest`, which uses `fake_backend_1, only runs one inline test runner

  $ dune build --display short @test1/runtest 2>&1 | grep alias
  inline_test_runner_test_lib1 alias test1/runtest

See that `test2/runtest`, which uses `fake_backend_2`, runs one inline test runner per partition

  $ dune build --display short @test2/runtest 2>&1 | grep alias
  inline_test_runner_test_lib2 alias test2/runtest
  inline_test_runner_test_lib2 alias test2/runtest
  inline_test_runner_test_lib2 alias test2/runtest

See that we indeed have 3 partitions

  $ cat _build/default/test2/.test_lib2.inline-tests/partitions-best
  p1
  p2
  p3


  $ dune build --display short @test3/runtest 2>&1 | grep alias
  [1]

See that we have no partition.

  $ cat _build/default/test3/.test_lib3.inline-tests/partitions-best
