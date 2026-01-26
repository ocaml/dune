  $ dune build --display short --profile unsound --debug-dependency-path @all
          rocq .basic.theory.d
          rocq Nbasic_foo.{cmi,cmxs},foo.{glob,vo}
          rocq Nbasic_bar.{cmi,cmxs},bar.{glob,vo}
