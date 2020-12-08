  $ dune build --debug-dependency-path
  File "a/dune", line 2, characters 7-8:
  2 |  (name a)
             ^
  Error: Cycle found
  - b
  - a
  - b
  -> required by a/a.v.d
  -> required by a/a.vo
  -> required by install lib/coq/user-contrib/a/a.vo
  -> required by ccycle.install
  -> required by alias default
  -> required by alias default
  [1]
