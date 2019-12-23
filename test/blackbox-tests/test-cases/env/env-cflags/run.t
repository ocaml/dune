  $ dune printenv --profile default . | tr -s "\n" " "
   ((flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags ($flags ":standard + in .")) (cxx_flags ($flags ":standard + in ."))) 

  $ dune printenv --profile default src | tr -s "\n" " "
   ((flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags ($flags ":standard + in ." ":standard + in src")) (cxx_flags ($flags ":standard + in ." ":standard + in src"))) 

  $ dune printenv --profile default bin
  
   ((flags
     (-w -40))
    (ocamlc_flags (-g))
    (ocamlopt_flags (-g))
    (c_flags ("in bin"))
    (cxx_flags ("in bin")))
  
  $ dune printenv --profile default run
  
   ((flags
     (-w -40))
    (ocamlc_flags (-g))
    (ocamlopt_flags (-g))
    (c_flags (-DTEST_C))
    (cxx_flags (-DTEST_CPP)))
  
  $ dune exec --profile default ./run/foo.exe
  TEST_C defined.

  $ dune exec --profile default ./run/bar.exe
  TEST_CPP defined.

