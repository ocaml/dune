  $ dune build --display short --debug-dependency-path
  File "public/dune", line 4, characters 11-18:
  4 |  (theories private))
                 ^^^^^^^
  Error: Theory "private" is private, it cannot be a dependency of a public
  theory. You need to associate "private" to a package.
  -> required by public/b.v.d
  -> required by public/b.vo
  -> required by install lib/coq/user-contrib/public/b.vo
  -> required by public.install
  -> required by alias default
  -> required by alias default
  [1]
