copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf/exe-foo
  $ dune ocaml-merlin --dump-config=$(pwd)/copy_files |
  > grep -B 1 -A 0 "pp"
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp.exe))
