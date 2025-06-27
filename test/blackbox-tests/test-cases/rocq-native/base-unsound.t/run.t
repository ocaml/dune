  $ dune build --display short --profile unsound --debug-dependency-path @all
        coqdep .basic.theory.d
          coqc Nbasic_foo.{cmi,cmxs},foo.{glob,vo}
          coqc Nbasic_bar.{cmi,cmxs},bar.{glob,vo}
