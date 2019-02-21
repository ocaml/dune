  $ ./printenv.sh --profile default . 
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (STANDARD_C_FLAGS ":standard + in .")) (cxx_flags (STANDARD_CXX_FLAGS ":standard + in .")) ) 

  $ ./printenv.sh --profile default src
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (STANDARD_C_FLAGS ":standard + in ." ":standard + in src")) (cxx_flags (STANDARD_CXX_FLAGS ":standard + in ." ":standard + in src")) ) 

  $ ./printenv.sh --profile default bin
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags ("in bin")) (cxx_flags ("in bin")) ) 
  $ ./printenv.sh --profile default run
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (-DTEST_C)) (cxx_flags (-DTEST_CPP)) ) 
  $ dune exec --profile default ./run/foo.exe
  TEST_C defined.

  $ dune exec --profile default ./run/bar.exe
  TEST_CPP defined.

