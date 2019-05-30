copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin
  $ cat copy_files/.merlin | grep "FLG -pp"
  FLG -pp '$TESTCASE_ROOT/_build/default/pp.exe'
