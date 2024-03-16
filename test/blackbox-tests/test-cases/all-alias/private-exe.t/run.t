@all builds private exe's

  $ dune build @all --display short 2>&1 | grep -i exe
      ocamlopt foo.exe
