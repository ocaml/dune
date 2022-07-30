We check cycles are detected
  $ dune build
  Error: Dependency cycle between:
     theory A in A
  -> theory B in B
  -> theory A in A
  -> required by _build/default/A/a.v.d
  -> required by _build/default/A/a.glob
  -> required by alias A/all
  -> required by alias default
  [1]
