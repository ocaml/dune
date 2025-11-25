Testing the _RocqProject generation.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (generate_project_file))
  > EOF

  $ touch foo.v

  $ dune build
  $ [ -f _RocqProject ]
