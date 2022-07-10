  $ dune build --display short --profile unsound --debug-dependency-path @all
          coqc foo.{glob,vo}
          coqc bar.{glob,vo}
