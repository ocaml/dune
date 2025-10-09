Test mix of regular files, directories, and symlinks.

A realistic scenario with mixed content types: regular files, directories, file
symlinks, and directory symlinks.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d/regular_dir)
  >    (system "echo '' > d/regular_file.txt")
  >    (system "echo '' > d/regular_dir/nested.txt")
  >    (chdir d
  >     (progn
  >      (run ln -s regular_file.txt file_symlink.txt)
  >      (run ln -s regular_dir dir_symlink))))))
  > EOF

  $ dune build d

  $ ls _build/default/d
  dir_symlink
  file_symlink.txt
  regular_dir
  regular_file.txt

  $ cat _build/default/d/dir_symlink/nested.txt
  
