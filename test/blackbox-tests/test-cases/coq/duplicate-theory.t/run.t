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
  Error: Coq theory foo is defined twice:
  - dune:5
  - dune:2
  [1]
