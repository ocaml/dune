Testing the _CoqProject generation.

  $ make_rocq_project 3.21 0.11

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules_flags (foo (:standard -flag)))
  >  (generate_project_file))
  > EOF

  $ touch foo.v

  $ dune build
  File "dune", line 4, characters 1-24:
  4 |  (generate_project_file))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: (generate_project_file) is not compatible with (modules_flags ...)
  [1]
