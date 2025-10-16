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
