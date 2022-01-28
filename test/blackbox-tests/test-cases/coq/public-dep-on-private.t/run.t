  $ dune build --display short --debug-dependency-path
  File "public/dune", line 4, characters 11-18:
  4 |  (theories private))
                 ^^^^^^^
  Error: Theory "private" is private, it cannot be a dependency of a public
  theory. You need to associate "private" to a package.
  -> required by _build/default/public/b.v.d
  -> required by _build/default/public/b.vo
  -> required by _build/install/default/lib/coq/user-contrib/public/b.vo
  -> required by _build/default/public.install
  -> required by %{read:public.install} at dune:3
  -> required by alias default in dune:1
  [1]
