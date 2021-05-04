Test case for coq.theory and the env stanza to set coq flags

  $ cat > foo.v <<EOF
  > (* This will only compile with -type-in-type *)
  > Definition t := Type.
  > Definition false : t := t.
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using coq 0.3)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name basic))
  > (env (unsound (coq (flags -type-in-type))))
  > EOF

  $ dune build @all
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]
  $ dune build @all --profile unsound
  File "./foo.v", line 3, characters 24-25:
  Error:
  The term "t" has type "Type" while it is expected to have type 
  "t" (universe inconsistency: Cannot enforce t.u0 < t.u0 because t.u0 = t.u0).
  
  [1]
