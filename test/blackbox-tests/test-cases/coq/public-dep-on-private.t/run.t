  $ dune build --display short --debug-dependency-path
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  File "public/dune", line 4, characters 11-18:
  4 |  (theories private))
                 ^^^^^^^
  Theory "private" is private, it cannot be a dependency of a public theory.
  You need to associate "private" to a package.
  -> required by theory public in public/dune:2
  -> required by _build/default/public/b.glob
  -> required by _build/install/default/lib/coq/user-contrib/public/b.glob
  -> required by _build/default/public.install
  -> required by %{read:public.install} at dune:3
  -> required by alias default in dune:1
  [1]
