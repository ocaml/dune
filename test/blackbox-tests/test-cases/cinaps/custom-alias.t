Custom alias for the cinaps

  $ make_cinaps_project 3.7 1.2

  $ cat > dune <<EOF
  > (cinaps
  >  (files foo.ml)
  >  (alias foo))
  > EOF

  $ touch foo.ml

  $ dune build @foo --display short 2>&1 | grep alias
        cinaps alias foo
