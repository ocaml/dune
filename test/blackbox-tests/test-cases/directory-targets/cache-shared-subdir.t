We create 2 directory targets which share a whole subdirectory.

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled
  $ . ./helpers.sh

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (target (dir d1))
  >  (action
  >   (progn
  >    (run ./gen.sh d1/shared1)
  >    (no-infer
  >     (write-file d1/x contents_x)))))
  > 
  > (rule
  >  (target (dir d2))
  >  (action
  >   (progn
  >    (run ./gen.sh d2/shared2)
  >    (no-infer
  >     (write-file d2/y contents_y)))))
  > EOF

  $ cat > gen.sh << 'EOF'
  > #!/usr/bin/env sh
  > out=$1
  > mkdir -p $out
  > echo contents_a > $out/a
  > echo contents_b > $out/b
  > EOF
  $ chmod +x gen.sh

  $ dune build d1/ d2/

We expect the targets to be linked in the shared cache.

  $ is_linked _build/default/d1/shared1/a
  linked
  $ is_linked _build/default/d1/shared1/b
  linked
  $ is_linked _build/default/d2/shared2/a
  linked
  $ is_linked _build/default/d2/shared2/b
  linked
