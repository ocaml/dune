Test for https://github.com/ocaml/dune/issues/7893
When using an installed theory with plugins, things should work fine.

  $ dune build --root to_install @all
  Entering directory 'to_install'
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Hello
  Leaving directory 'to_install'
  $ dune install --root to_install --prefix=$PWD

We now build the normal theory, and should work

  $ COQPATH=$PWD/lib/coq/user-contrib dune build --root user @all
  Entering directory 'user'
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Hello
  Leaving directory 'user'
