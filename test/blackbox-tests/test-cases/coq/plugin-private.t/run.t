In Coq >= 0.6, depending on a private library as a plugin is an error.
  $ cat > dune << EOF
  > (library
  >  (name foo))
  > 
  > (coq.theory
  >  (name bar)
  >  (plugins foo))
  > EOF

  $ dune build
  File "dune", line 6, characters 10-13:
  6 |  (plugins foo))
                ^^^
  Error: Using private library foo as a Coq plugin is not supported
  [1]
