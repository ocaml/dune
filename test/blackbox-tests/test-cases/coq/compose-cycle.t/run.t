  $ dune build --debug-dependency-path
  File "a/dune", line 2, characters 7-8:
  2 |  (name a)
             ^
  Error: Cycle found
  - b
  - a
  - b
  -> required by _build/default/a/a.v.d
  -> required by _build/default/a/a.vo
  -> required by _build/install/default/lib/coq/user-contrib/a/a.vo
  -> required by _build/default/ccycle.install
  -> required by %{read:ccycle.install} at dune:3
  -> required by alias default in dune:1
  File "b/dune", line 2, characters 7-8:
  2 |  (name b)
             ^
  Error: Cycle found
  - a
  - b
  - a
  -> required by _build/default/b/b.v.d
  -> required by _build/default/b/b.vo
  -> required by _build/install/default/lib/coq/user-contrib/b/b.vo
  -> required by _build/default/ccycle.install
  -> required by %{read:ccycle.install} at dune:3
  -> required by alias default in dune:1
  [1]
