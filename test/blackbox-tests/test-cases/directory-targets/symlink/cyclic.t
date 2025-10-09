Test cyclic symlink (should fail with appropriate error).

A symlink that points to itself, creating a cycle.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d)
  >    (chdir d
  >     (run ln -s cycle cycle)))))
  > EOF

  $ dune build d
  File "dune", lines 1-7, characters 0-105:
  1 | (rule
  2 |  (target (dir d))
  3 |  (action
  4 |   (progn
  5 |    (run mkdir -p d)
  6 |    (chdir d
  7 |     (run ln -s cycle cycle)))))
  Error: Error trying to read targets after a rule was run:
  - d/cycle: Cyclic symbolic link
  [1]
