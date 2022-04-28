Attempt to create a directory with chdir + with-stdout-to:

  $ cat > dune-project <<EOF
  > (lang dune 3.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (deps (sandbox always))
  >  (action
  >   (progn
  >    (chdir output
  >     (with-stdout-to x (echo foobar))))))
  > EOF

  $ dune build foobar/
  File "dune", line 1, characters 0-130:
  1 | (rule
  2 |  (targets (dir output))
  3 |  (deps (sandbox always))
  4 |  (action
  5 |   (progn
  6 |    (chdir output
  7 |     (with-stdout-to x (echo foobar))))))
  Error: Rule has targets in different directories.
  Targets:
  - output/x
  - output
  [1]
