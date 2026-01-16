  $ dune build --display short --profile unsound --debug-dependency-path @all
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .basic.theory.d
          coqc foo.{glob,vo}
          coqc bar.{glob,vo}
