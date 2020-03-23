One can use dune expand-targets to get the _build path to the provided targets

  $ dune expand-targets --root simple %{cmi:a} %{cmx:a}
  Entering directory 'simple'
  _build/default/.a.objs/byte/a.cmi
  _build/default/.a.objs/native/a.cmx
