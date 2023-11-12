Duplicate theories should be caught by Dune:

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo))
  > 
  > (coq.theory
  >  (name foo))
  > EOF

BUG The directory target is found before the theory

  $ dune build
  File "dune", line 1, characters 0-24:
  1 | (coq.theory
  2 |  (name foo))
  Error: The following both define the same directory target:
  _build/default/foo.tex
