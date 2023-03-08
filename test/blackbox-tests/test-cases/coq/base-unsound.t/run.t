  $ dune build --display short --profile unsound --debug-dependency-path @all
        coqdep .basic.theory.d
          coqc foo.{glob,vo}
          coqc bar.{glob,vo}
