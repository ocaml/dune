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
  Error: the following both define the directory target: _build/default/foo.tex
  - dune:1
  - dune:4
  [1]
