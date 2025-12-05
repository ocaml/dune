Testing the _RocqProject generation.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

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
