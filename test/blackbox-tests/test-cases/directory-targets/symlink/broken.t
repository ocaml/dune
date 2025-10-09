Test broken symlink (should fail with appropriate error).

Broken symlinks should be detected and reported.

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
  >     (run ln -s nonexistent broken_link)))))
  > EOF

  $ dune build d
  File "dune", lines 1-7, characters 0-117:
  1 | (rule
  2 |  (target (dir d))
  3 |  (action
  4 |   (progn
  5 |    (run mkdir -p d)
  6 |    (chdir d
  7 |     (run ln -s nonexistent broken_link)))))
  Error: Error trying to read targets after a rule was run:
  - d/broken_link: Broken symbolic link
  [1]
