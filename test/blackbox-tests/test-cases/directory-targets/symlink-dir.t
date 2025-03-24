This demonstrates what happens when a directory target contains a symlink that
points to a directory.

See #9873.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d)
  >    (chdir d
  >     (progn
  >      (run mkdir a)
  >      (run touch a/x.txt)
  >      (run ln -s a b))))))
  > EOF

  $ dune build d
  File "dune", lines 1-10, characters 0-154:
   1 | (rule
   2 |  (target (dir d))
   3 |  (action
   4 |   (progn
   5 |    (run mkdir -p d)
   6 |    (chdir d
   7 |     (progn
   8 |      (run mkdir a)
   9 |      (run touch a/x.txt)
  10 |      (run ln -s a b))))))
  Error: Error trying to read targets after a rule was run:
  - d/b: Unexpected file kind "S_DIR" (directory)
  [1]
