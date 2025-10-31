Symlinks to directories within the same directory target now work.

This is the original test case for #9873, which has been fixed.

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

  $ ls _build/default/d
  a
  b

  $ readlink _build/default/d/b
  a

  $ cat _build/default/d/b/x.txt

