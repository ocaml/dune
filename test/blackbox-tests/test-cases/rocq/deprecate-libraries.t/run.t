The libraries field is deprecated
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (rocq.theory
  >  (name bar)
  >  (libraries bar.foo))
  > EOF

  $ dune build
  File "dune", line 7, characters 2-11:
  7 |  (libraries bar.foo))
        ^^^^^^^^^
  Error: Unknown field "libraries"
  [1]

Having both a libraries and plugins field is an error
  $ cat > dune << EOF
  > (library
  >  (public_name bar.foo)
  >  (name foo))
  > 
  > (rocq.theory
  >  (name bar)
  >  (libraries bar.foo)
  >  (plugins bar.foo))
  > EOF

  $ dune build
  File "dune", line 7, characters 2-11:
  7 |  (libraries bar.foo)
        ^^^^^^^^^
  Error: Unknown field "libraries"
  [1]

