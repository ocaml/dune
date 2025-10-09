Test relative symlink to directory within the same directory target.

This is the basic case from #9873 - a symlink pointing to another directory
within the same directory target.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d/actual_dir)
  >    (system "echo '' > d/actual_dir/file.txt")
  >    (chdir d
  >     (run ln -s actual_dir symlink_to_dir)))))
  > EOF

  $ dune build d
  File "dune", lines 1-8, characters 0-176:
  1 | (rule
  2 |  (target (dir d))
  3 |  (action
  4 |   (progn
  5 |    (run mkdir -p d/actual_dir)
  6 |    (system "echo '' > d/actual_dir/file.txt")
  7 |    (chdir d
  8 |     (run ln -s actual_dir symlink_to_dir)))))
  Error: Error trying to read targets after a rule was run:
  - d/symlink_to_dir: Unexpected file kind "S_DIR" (directory)
  [1]

  $ ls _build/default/d
  actual_dir
  symlink_to_dir

  $ cat _build/default/d/symlink_to_dir/file.txt
  
