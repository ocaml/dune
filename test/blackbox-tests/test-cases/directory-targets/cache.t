We test that directory targets can go in the shared cache. See #8067.

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled

In project a, we create a rule with a directory target. The script that creates
the target displays a message.

  $ mkdir a
  $ cd a
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

We run, and expect the error message to be displayed, the targets to be
created, and the cache to be populated.

  $ dune build out
  Running create.sh
  $ dune_cmd exists _build/default/out/a
  true
  $ dune_cmd exists _build/default/out/b
  true
  $ dune cache size
  0B

Now we create another project with the same contents.

  $ cd ..
  $ mkdir b
  $ cp a/dune-project a/dune a/create.sh b/
  $ cd b

When building, we expect to restore from cache. So the message should not be
displayed, but the files should be created.

  $ dune build out
  $ dune_cmd exists _build/default/out/a
  true
  $ dune_cmd exists _build/default/out/b
  true

If part of a directory target is removed, we expect it to be rebuilt using the
cache.

  $ dune_cmd wait-for-fs-clock-to-advance
  $ rm _build/default/out/a
  $ dune build out
  $ dune_cmd exists _build/default/out/a
  true
