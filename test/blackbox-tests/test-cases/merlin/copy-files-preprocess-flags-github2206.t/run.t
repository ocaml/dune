copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf/exe-foo
  $ dune ocaml merlin dump-config --format=json $PWD/copy_files | jq -r '
  >   include "dune";
  >   merlinEntry("Foo")
  >   | merlinConfigItemsNamed(["FLG"])
  >   | select(.[0] == "FLG" and (.[1] | index("-pp")))
  >   | @json'
  ["FLG",["-pp","$TESTCASE_ROOT/_build/default/pp.exe"]]
  ["FLG",["-pp","$TESTCASE_ROOT/_build/default/pp.exe"]]
