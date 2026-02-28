  $ dune build @install

  $ dune trace cat | jq -c '
  > include "dune";
  >   select(.cat == "process" and .name == "start")
  > | .args
  > | targets
  > | select(length > 0)
  > '
  ["_build/default/a1/.a.objs/byte/a.cmi","_build/default/a1/.a.objs/byte/a.cmo","_build/default/a1/.a.objs/byte/a.cmt"]
  ["_build/default/a2/.a.objs/byte/a.cmi","_build/default/a2/.a.objs/byte/a.cmo","_build/default/a2/.a.objs/byte/a.cmt"]
  ["_build/default/a1/.a.objs/native/a.cmx","_build/default/a1/.a.objs/native/a.o"]
  ["_build/default/a1/a.cma"]
  ["_build/default/a2/.a.objs/native/a.cmx","_build/default/a2/.a.objs/native/a.o"]
  ["_build/default/a2/a.cma"]
  ["_build/default/a1/a.a","_build/default/a1/a.cmxa"]
  ["_build/default/a2/a.a","_build/default/a2/a.cmxa"]
  ["_build/default/a1/a.cmxs"]
  ["_build/default/a2/a.cmxs"]
