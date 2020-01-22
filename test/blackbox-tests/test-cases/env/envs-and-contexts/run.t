Regression test for https://github.com/ocaml/dune/issues/1016#issuecomment-411390740

  $ dune printenv
  Environment for context dev:
    
     ((flags (dev-flags))
      (ocamlc_flags (-g))
      (ocamlopt_flags (-g))
      (c_flags
       ($flags))
      (cxx_flags
       ($flags))
      (menhir_flags ()))
  
    
  Environment for context release:
    
     ((flags (release-flags))
      (ocamlc_flags (-g))
      (ocamlopt_flags (-g))
      (c_flags
       ($flags))
      (cxx_flags
       ($flags))
      (menhir_flags ()))
  
    
