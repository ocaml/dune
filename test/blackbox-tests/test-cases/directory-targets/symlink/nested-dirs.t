Test nested directory with symlinks.

Directory target containing subdirectories with symlinks to other subdirectories.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d/sub1/sub2 d/other)
  >    (system "echo '' > d/sub1/sub2/file.txt")
  >    (chdir d/other
  >     (run ln -s ../sub1/sub2 link_to_sub2)))))
  > EOF

  $ dune build d
  File "dune", lines 1-8, characters 0-188:
  1 | (rule
  2 |  (target (dir d))
  3 |  (action
  4 |   (progn
  5 |    (run mkdir -p d/sub1/sub2 d/other)
  6 |    (system "echo '' > d/sub1/sub2/file.txt")
  7 |    (chdir d/other
  8 |     (run ln -s ../sub1/sub2 link_to_sub2)))))
  Error: Error trying to read targets after a rule was run:
  - d/other/link_to_sub2: Unexpected file kind "S_DIR" (directory)
  [1]

  $ ls _build/default/d/other
  link_to_sub2

  $ cat _build/default/d/other/link_to_sub2/file.txt
  
