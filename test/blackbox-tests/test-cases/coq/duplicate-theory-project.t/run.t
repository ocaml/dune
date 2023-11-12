Duplicate theories in the same project should be caught by Dune:

  $ mkdir A
  $ mkdir B

  $ cat > A/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ cat > B/dune << EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ dune build
  Error: Coq theory foo is defined twice:
  - theory foo in B/dune:2
  - theory foo in A/dune:2
  -> required by _build/default/A/.foo.theory.d
  -> required by alias A/all
  -> required by alias default
  [1]
