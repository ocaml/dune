  $ dune build --display short --profile unsound --debug-dependency-path @all
          rocq .basic.theory.d
          rocq foo.{glob,vo}
          rocq bar.{glob,vo}
