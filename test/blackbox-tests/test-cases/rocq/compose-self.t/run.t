Composing a theory with itself should cause a cycle
  $ dune build
  Error: Dependency cycle between:
     theory A in A/dune:2
  -> required by _build/default/A/.A.theory.d
  -> required by alias A/all
  -> required by alias default
  [1]
