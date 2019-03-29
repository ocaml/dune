  $ dune build --root base --display short --debug-dependency-path @all
  Entering directory 'base'
        coqdep bar.v.d
        coqdep foo.v.d
          coqc foo.vo
          coqc bar.vo

  $ dune build --root rec_module --display short --debug-dependency-path @all
  Entering directory 'rec_module'
