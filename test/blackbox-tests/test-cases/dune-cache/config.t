Test cache configuration.

Check that old cache configuration format works fine with an old language

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > (cache-transport direct)
  > (cache-duplication copy)
  > (cache-trim-period 1h)
  > (cache-trim-size 1GB)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (copy source target)))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

Test that DUNE_CACHE_ROOT can be used to control the cache location

  $ export DUNE_CACHE_ROOT=$PWD/.cache

Build succeeds and the 'copy' mode is respected

  $ dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  1

Switch to the 'hardlink' mode now

  $ cat > config <<EOF
  > (lang dune 2.1)
  > (cache enabled)
  > (cache-transport direct)
  > (cache-duplication hardlink)
  > (cache-trim-period 1h)
  > (cache-trim-size 1GB)
  > EOF

Build succeeds and the 'hardlink' mode is respected

  $ rm -rf _build/default
  $ dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  3

Now repeat the tests with the old configuration format but 3.0 language

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-transport direct)
  > (cache-duplication copy)
  > (cache-trim-period 1h)
  > (cache-trim-size 1GB)
  > EOF

Build fails because 'cache-transport' was deleted

  $ dune build --config-file config target
  File "$TESTCASE_ROOT/config", line 3, characters 0-24:
  3 | (cache-transport direct)
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'cache-transport' was deleted in version 3.0 of the dune language.
  Dune cache now uses only the direct transport mode.
  [1]

So, we comply and delete it

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-duplication copy)
  > (cache-trim-period 1h)
  > (cache-trim-size 1GB)
  > EOF

Build fails because 'cache-duplication' was renamed

  $ dune build --config-file config target
  File "$TESTCASE_ROOT/config", line 3, characters 0-24:
  3 | (cache-duplication copy)
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'cache-duplication' was renamed to 'cache-storage-mode' in the 3.0
  version of the dune language
  [1]

So, we comply and rename it.

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode copy)
  > (cache-trim-period 1h)
  > (cache-trim-size 1GB)
  > EOF

Build fails because 'cache-trim-period' was deleted

  $ dune build --config-file config target
  File "$TESTCASE_ROOT/config", line 4, characters 0-22:
  4 | (cache-trim-period 1h)
      ^^^^^^^^^^^^^^^^^^^^^^
  Error: 'cache-trim-period' was deleted in version 3.0 of the dune language.
  To trim the cache, use the 'dune cache trim' command.
  [1]

So, we comply and delete it

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode copy)
  > (cache-trim-size 1GB)
  > EOF

Build fails because 'cache-trim-size' was deleted

  $ dune build --config-file config target
  File "$TESTCASE_ROOT/config", line 4, characters 0-21:
  4 | (cache-trim-size 1GB)
      ^^^^^^^^^^^^^^^^^^^^^
  Error: 'cache-trim-size' was deleted in version 3.0 of the dune language. To
  trim the cache, use the 'dune cache trim' command.
  [1]

So, we comply and delete it

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode copy)
  > EOF

Build succeeds and the 'copy' mode is respected

  $ rm -rf _build/default
  $ dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  1

Switch to the 'hardlink' mode now

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode hardlink)
  > EOF

Build succeeds and the 'hardlink' mode is respected

  $ rm -rf _build/default
  $ dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  3

Let's disable the cache

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache disabled)
  > (cache-storage-mode hardlink)
  > EOF

Test that in this mode the shared cache directory is not created

  $ rm -rf $DUNE_CACHE_ROOT
  $ rm -rf _build/default
  $ dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  1
  $ dune_cmd exists $DUNE_CACHE_ROOT
  false

Test that the cache can be enabled via the environment variable

  $ rm -rf _build/default
  $ DUNE_CACHE=enabled dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  3
  $ dune_cmd exists $DUNE_CACHE_ROOT
  true

Test that we can override the environment variable from the command line

  $ rm -rf $DUNE_CACHE_ROOT
  $ rm -rf _build/default
  $ DUNE_CACHE=enabled dune build --config-file config target --cache=disabled
  $ dune_cmd stat hardlinks _build/default/target
  1
  $ dune_cmd exists $DUNE_CACHE_ROOT
  false

Test that we can override the storage mode via the environment variable

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-storage-mode hardlink)
  > EOF

  $ rm -rf _build/default
  $ DUNE_CACHE_STORAGE_MODE=copy dune build --config-file config target
  $ dune_cmd stat hardlinks _build/default/target
  1

Test that we can override the environment variable from the command line

  $ rm -rf $DUNE_CACHE_ROOT
  $ rm -rf _build/default
  $ DUNE_CACHE_STORAGE_MODE=copy dune build --config-file config target --cache-storage-mode=hardlink
  $ dune_cmd stat hardlinks _build/default/target
  3
