  $ dune build --display short --profile unsound --debug-dependency-path @all
        coqdep bar.v.d
        coqdep foo.v.d
          coqc .foo.aux,foo.vo
          coqc .bar.aux,bar.vo
