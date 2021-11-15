  $ dune build --debug-dependency-path
  File "b/dune", line 4, characters 11-12:
  4 |  (theories a))
                 ^
  Error: Theory a not found
  -> required by _build/default/b/b.v.d
  -> required by _build/default/b/b.vo
  -> required by _build/install/default/lib/coq/user-contrib/b/b.vo
  -> required by _build/default/cvendor.install
  -> required by %{read:cvendor.install} at dune:3
  -> required by alias default in dune:1
  [1]
