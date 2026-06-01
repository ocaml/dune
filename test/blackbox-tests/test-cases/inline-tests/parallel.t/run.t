First, build silently to avoid some noise

  $ dune build

See that `test1/runtest`, which uses `fake_backend_1, only runs one inline test runner

  $ dune build --display short @test1/runtest 2>&1 | grep alias
  inline-test-runner alias test1/runtest-test_lib1

See that `test2/runtest`, which uses `fake_backend_2`, runs one inline test runner per partition

  $ dune build --display short @test2/runtest 2>&1 | grep alias
  inline-test-runner alias test2/runtest-test_lib2
  inline-test-runner alias test2/runtest-test_lib2
  inline-test-runner alias test2/runtest-test_lib2

See that the trace reports 3 partitions

  $ dune trace cat | jq_dune -c 'inlineTestPartitions'
  {"library":"test_lib2","mode":"best","partitions":["p1","p2","p3"]}


  $ dune build --display short @test3/runtest 2>&1 | grep alias
  [1]

See that the trace reports no partition.

  $ dune trace cat | jq_dune -c 'inlineTestPartitions'
  {"library":"test_lib3","mode":"best","partitions":[]}
