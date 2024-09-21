This test checks that the `dune cache size` command returns the correct size of
the cache.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ cat > config << EOF
  > (lang dune 3.7)
  > (cache enabled)
  > (cache-storage-mode copy)
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 3.7)
  > EOF

  $ cat > dune << EOF
  > (rule
  > (targets target_a)
  > (action
  >  (with-outputs-to
  >   target_a
  >   (echo Hello World!))))
  > EOF

We build a simple file with the contents of "Hello World!".

  $ dune build target_a --display=short

Now we remove it so that we are checking the size of the file rather than the
link Dune created.

  $ rm _build/default/target_a

The size command reports the size of the cache in bytes in human-readable form.
It correctly reports 12 bytes.

  $ dune cache size
  12B

We also have a machine-readable version of the command which reports the size of
the cache in bytes directly without any units.

  $ dune cache size --machine-readable
  12
