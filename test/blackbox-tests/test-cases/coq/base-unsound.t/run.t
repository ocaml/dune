  $ dune build --display short --profile unsound --debug-dependency-path @all
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
        coqdep .basic.theory.d
          coqc foo.{glob,vo}
          coqc bar.{glob,vo}
