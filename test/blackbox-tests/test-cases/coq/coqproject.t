Testing the _CoqProject generation.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using coq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name a)
  >  (generate_project_file))
  > EOF

  $ touch foo.v

  $ dune build
  $ [ -f _CoqProject ]
