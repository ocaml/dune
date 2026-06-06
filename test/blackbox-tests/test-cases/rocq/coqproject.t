Testing the _RocqProject generation.

  $ make_rocq_project 3.21 0.11

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (generate_project_file))
  > EOF

  $ touch foo.v

  $ dune build
  $ [ -f _RocqProject ]
