This test makes sure that whenever we evaluate a glob inside a directory target
that matches nothing, we still copy the directory and make it empty.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (system "mkdir output && touch output/foo.txt")))
  > (rule
  >  (targets x)
  >  (deps (glob_files output/*.baz))
  >  (action (system "ls output/ && touch x")))
  > EOF

  $ DUNE_SANDBOX=copy dune build x
