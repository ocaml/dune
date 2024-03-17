Custom alias for the cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using cinaps 1.2)
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files foo.ml)
  >  (alias foo))
  > EOF

  $ touch foo.ml

  $ dune build @foo --display short 2>&1 | grep alias
        cinaps alias foo
