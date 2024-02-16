Duplicate mld's in the same scope
  $ dune build @doc-new
  Error: Package root has two mld's with the same basename
  _build/default/lib1/test.mld, _build/default/lib2/test.mld
  -> required by alias doc-new
  [1]
