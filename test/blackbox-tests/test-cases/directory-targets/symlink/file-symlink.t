Test symlink to file within the same directory target.

Symlinks to files within the same directory target should work correctly.

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
  >    (system "echo '' > d/original.txt")
  >    (chdir d
  >     (run ln -s original.txt link.txt)))))
  > EOF

  $ dune build d

  $ ls _build/default/d
  link.txt
  original.txt

  $ cat _build/default/d/link.txt
  

