Symlinks in directory targets should go in the shared cache.

See #11523.

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled

  $ make_directory_targets_project 3.0

  $ cat > dune << EOF
  > (rule
  >  (target (dir d))
  >  (action
  >   (progn
  >    (run mkdir -p d)
  >    (chdir d
  >     (progn
  >      (echo building)
  >      (run touch target)
  >      (run ln -s target symlink))))))
  > EOF

  $ dune build d
  building

  $ dune clean

  $ dune build d
  building
