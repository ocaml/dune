Testing the warnings field of the coq.theory

Disabling a warning
  $ cat > dune << EOF
  > (coq.theory
  >  (name warn)
  >  (warnings
  >   -deprecated-syntactic-definition))
  > EOF
  $ dune build

Enabling a warning
  $ cat > dune << EOF
  > (coq.theory
  >  (name warn)
  >  (warnings
  >   +deprecated-syntactic-definition))
  > EOF
  $ dune build
  File "./warn.v", line 7, characters 18-21:
  Error: Notation dep is deprecated since 8.15. Hello from Coq!
  [deprecated-syntactic-definition,deprecated]
  
  [1]

Behaviour for bogus warning
  $ cat > dune << EOF
  > (coq.theory
  >  (name warn)
  >  (warnings
  >   -deprecated-syntactic-definition
  >   -non-existant-warning))
  > EOF
  $ dune build

Empty warnings
  $ cat > dune << EOF
  > (coq.theory
  >  (name warn))
  > EOF
  $ dune build
  File "./warn.v", line 7, characters 18-21:
  Warning: Notation dep is deprecated since 8.15. Hello from Coq!
  [deprecated-syntactic-definition,deprecated]
