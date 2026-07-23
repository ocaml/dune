Testing the _RocqProject generation.

  $ make_rocq_project 3.21 0.11

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules foo)
  >  (generate_project_file))
  > 
  > (rocq.theory
  >  (name b)
  >  (modules bar)
  >  (generate_project_file))
  > EOF

  $ touch foo.v bar.v

  $ dune build
  Error: Multiple rules generated for _build/default/_RocqProject:
  - dune:6
  - dune:1
  [1]
