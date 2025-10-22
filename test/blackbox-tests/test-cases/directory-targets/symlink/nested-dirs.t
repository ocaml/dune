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

  $ ls _build/default/d/other
  link_to_sub2

  $ cat _build/default/d/other/link_to_sub2/file.txt
  
