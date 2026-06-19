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

The @rocqproject alias builds all _RocqProject files recursively.

  $ mkdir sub
  $ cat > sub/dune <<EOF
  > (rocq.theory
  >  (name b)
  >  (generate_project_file))
  > EOF

  $ touch sub/bar.v

  $ dune clean
  $ [ ! -f _RocqProject ]
  $ [ ! -f sub/_RocqProject ]
  $ dune build @rocqproject
  $ [ -f _RocqProject ]
  $ [ -f sub/_RocqProject ]
