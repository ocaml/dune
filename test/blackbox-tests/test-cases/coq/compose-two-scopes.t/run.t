  $ dune build --debug-dependency-path
  File "b/dune", line 4, characters 11-12:
  4 |  (theories a))
                 ^
  Error: Theory a not found
  -> required by b/b.v.d
  -> required by b/b.vo
  -> required by install lib/coq/user-contrib/b/b.vo
  -> required by cvendor.install
  -> required by alias default
  -> required by alias default
  [1]
