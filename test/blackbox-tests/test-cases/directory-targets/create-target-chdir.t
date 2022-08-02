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
  File "dune", line 7, characters 20-21:
  7 |     (with-stdout-to x (echo foobar))))))
                          ^
  Error: This action has targets in a different directory than the current one,
  this is not allowed by dune at the moment:
  - output/x
  [1]
