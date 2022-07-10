  $ dune build --display short --profile unsound --debug-dependency-path @all
          coqc .foo.aux,foo.{glob,vo}
          coqc .bar.aux,bar.{glob,vo}
