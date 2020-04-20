copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf
  $ cat _build/default/copy_files/.merlin-conf | grep "FLG -pp"
  FLG -pp '$TESTCASE_ROOT/_build/default/pp.exe'
