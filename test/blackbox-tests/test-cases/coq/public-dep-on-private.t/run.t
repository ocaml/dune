  $ dune build --display short --debug-dependency-path
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  File "public/dune", line 4, characters 11-18:
  4 |  (theories private))
                 ^^^^^^^
  Theory "private" is private, it cannot be a dependency of a public theory.
  You need to associate "private" to a package.
  -> required by theory public in public/dune:2
  -> required by _build/default/public/.public.theory.d
  -> required by _build/default/public/b.vo
  -> required by _build/install/default/lib/coq/user-contrib/public/b.vo
  -> required by _build/default/public.install
  -> required by %{read:public.install} at dune:3
  -> required by alias default in dune:1
  [1]
