Virtual libraries and preprocessed source
  $ dune build --sandbox symlink
  foo

The implementation module depends on the modules introduced by preprocessing
[vdep.mli], not on the module mentioned in the raw source.

  $ dune rules --format=json %{cmo:impl/use} \
  > | jq -r 'include "dune";
  >   .[]
  > | ruleDepFilePaths
  > | select(test("bar__(Dep_a|Dep_b|Raw_only)\\.cmi$"))' \
  > | sort
  _build/default/impl/.impl.objs/byte/bar__Dep_a.cmi
  _build/default/impl/.impl.objs/byte/bar__Dep_b.cmi

