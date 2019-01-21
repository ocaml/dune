  $ export STANDARD_C_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //'`
  $ export STANDARD_CXX_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //' | sed 's/-std=[^ ]* //' | sed 's/-std=[^$]*//'`
  $ dune printenv --profile default . | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (STANDARD_C_FLAGS ":standard + in .")) (cxx_flags (STANDARD_CXX_FLAGS ":standard + in .")) ) 

  $ dune printenv --profile default src | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (STANDARD_C_FLAGS ":standard + in ." ":standard + in src")) (cxx_flags (STANDARD_CXX_FLAGS ":standard + in ." ":standard + in src")) ) 

  $ dune printenv --profile default bin | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags ("in bin")) (cxx_flags ("in bin")) ) 
  $ dune printenv --profile default run | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
  ( (flags (-w -40)) (ocamlc_flags (-g)) (ocamlopt_flags (-g)) (c_flags (-DTEST_C)) (cxx_flags (-DTEST_CPP)) ) 
  $ dune exec --profile default ./run/foo.exe
  DTEST_C defined.
  [1]
  $ dune exec --profile default ./run/bar.exe
  DTEST_CPP defined.
  [1]
