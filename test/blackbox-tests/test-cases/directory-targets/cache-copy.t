We test that directory targets can go in the shared cache in copy mode.

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE_STORAGE_MODE=copy
  $ export DUNE_CACHE=enabled

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF
  $ cat > create.sh << EOF
  > #!/usr/bin/env sh
  > echo "Running create.sh"
  > mkdir out/
  > echo contents_a > out/a
  > echo contents_b > out/b
  > EOF
  $ chmod +x create.sh
  $ cat > dune << EOF
  > (rule
  >  (target (dir out))
  >  (action
  >   (run ./create.sh)))
  > EOF

  $ dune build out
  Running create.sh

  $ dune clean
  $ dune build out
