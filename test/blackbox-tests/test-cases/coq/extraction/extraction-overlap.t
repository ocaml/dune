The coq.extraction stanza should error if it overlaps with the coq.theory stanza

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > (using coq 0.8)
  > EOF

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo_theory))
  > 
  > (coq.extraction
  >  (prelude foo)
  >  (extracted_modules))
  > EOF

  $ cat > foo.v

  $ dune build
  Error: Coq module "foo" occurs in multiple Coq stanzas:
  - File "dune", line 4, characters 0-52:
    4 | (coq.extraction
    5 |  (prelude foo)
    6 |  (extracted_modules))
    
  - File "dune", line 1, characters 0-31:
    1 | (coq.theory
    2 |  (name foo_theory))
    
  [1]
