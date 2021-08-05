Building a cxx library should run ocamlmklib with "-lstdc++".
  $ dune build --verbose 2>&1 | grep -e "ocamlmklib" | grep -qe "-lstdc++"
