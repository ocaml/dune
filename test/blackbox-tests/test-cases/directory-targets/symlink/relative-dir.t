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

  $ ls _build/default/d
  actual_dir
  symlink_to_dir

  $ cat _build/default/d/symlink_to_dir/file.txt
  
