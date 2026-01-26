@all builds private libs

  $ dune build --display short @all 2>&1 | grep bar.cma
        ocamlc bar.cma
