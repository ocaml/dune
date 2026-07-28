First, build silently to avoid some noise

  $ dune build

See that `test1/runtest`, which uses `fake_backend_1, only runs one inline test runner

  $ dune build @test1/runtest
  $ dune trace cat | jq_dune -c 'inlineTestProcesses'
  {"prog":"inline-test-runner.bc","args":["--libname","test_lib1"],"exit":0}

See that `test2/runtest`, which uses `fake_backend_2`, runs one inline test runner per partition

  $ dune build @test2/runtest
  $ dune trace cat | jq_dune -c 'inlineTestProcesses'
  {"prog":"inline-test-runner.bc","args":["--libname","test_lib2","--list-partitions"],"exit":0}
  {"prog":"inline-test-runner.bc","args":["--libname","test_lib2","--partition","p1"],"exit":0}
  {"prog":"inline-test-runner.bc","args":["--libname","test_lib2","--partition","p2"],"exit":0}
  {"prog":"inline-test-runner.bc","args":["--libname","test_lib2","--partition","p3"],"exit":0}

See that the trace reports 3 partitions

  $ dune trace cat | jq_dune -c 'inlineTestPartitions'
  {"library":"test_lib2","mode":"byte","partitions":["p1","p2","p3"]}


  $ dune build @test3/runtest

See that the trace reports no partition.

  $ dune trace cat | jq_dune -c 'inlineTestPartitions'
  {"library":"test_lib3","mode":"byte","partitions":[]}
