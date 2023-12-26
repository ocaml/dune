Test reproducibility check

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (deps dep)
  >   (targets reproducible)
  >   (action (progn (echo "build reproducible\n")
  >                  (copy dep reproducible))))
  > (rule
  >   (targets non-reproducible)
  >   (action (no-infer (progn (echo "build non-reproducible\n")
  >                            (copy dep non-reproducible)))))
  > EOF

Both rules read [dep] but only the reproducible rule declares it as a dependency

  $ echo old-content > dep

Build both, which will store the results to the cache

  $ dune build --config-file config reproducible non-reproducible
  build reproducible
  build non-reproducible

Update the content and rebuild; only the reproducible rule will rerun

  $ echo new-content > dep
  $ dune build --config-file config reproducible non-reproducible
  build reproducible

When 'cache-check-probability' is unset, we skip reproducibility check

  $ rm -rf _build
  $ dune build --config-file config reproducible non-reproducible

Note that we didn't rerun the rules, since the results are in the cache

  $ dune_cmd cat _build/default/reproducible
  new-content
  $ dune_cmd cat _build/default/non-reproducible
  old-content

Set 'cache-check-probability' to 0.0, which should also skip the check

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-check-probability 0.0)
  > EOF
  $ rm -rf _build
  $ dune build --config-file config reproducible non-reproducible

Set 'cache-check-probability' to 1.0, which should trigger the check

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-check-probability 1.0)
  > EOF
  $ rm -rf _build
  $ dune build --config-file config reproducible non-reproducible
  Warning: cache store error [e4096cb8e17c59cb28c421878c60fbfa]: ((in_cache
  ((non-reproducible 1c8fc4744d4cef1bd2b8f5e915b36be9))) (computed
  ((non-reproducible 6cfaa7a90747882bcf4ffe7252c1cf89)))) after executing
  (echo 'build non-reproducible';cp dep non-reproducible)
  build reproducible
  build non-reproducible

Check that the reported digests make sense

  $ dune_cmd cat $DUNE_CACHE_ROOT/files/v4/1c/1c8fc4744d4cef1bd2b8f5e915b36be9
  old-content
  $ dune_cmd cat $DUNE_CACHE_ROOT/files/v4/6c/6cfaa7a90747882bcf4ffe7252c1cf89
  new-content

Check that probability values less than zero and greater than one are rejected

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-check-probability -0.1)
  > EOF
  $ dune build --config-file config reproducible non-reproducible
  File "$TESTCASE_ROOT/config", line 3, characters 0-30:
  3 | (cache-check-probability -0.1)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The reproducibility check probability must be in the range [0, 1].
  [1]

  $ cat > config <<EOF
  > (lang dune 3.0)
  > (cache enabled)
  > (cache-check-probability 3.14)
  > EOF
  $ dune build --config-file config reproducible non-reproducible
  File "$TESTCASE_ROOT/config", line 3, characters 0-30:
  3 | (cache-check-probability 3.14)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The reproducibility check probability must be in the range [0, 1].
  [1]

Test that the environment variable and the command line flag work too

  $ rm -rf _build
  $ DUNE_CACHE_CHECK_PROBABILITY=-1 dune build --cache=enabled reproducible non-reproducible
  Error: The reproducibility check probability must be in the range [0, 1].
  [1]

  $ rm -rf _build
  $ DUNE_CACHE_CHECK_PROBABILITY=0.0 dune build --cache=enabled reproducible non-reproducible

  $ rm -rf _build
  $ DUNE_CACHE_CHECK_PROBABILITY=1.0 dune build --cache=enabled reproducible non-reproducible
  Warning: cache store error [e4096cb8e17c59cb28c421878c60fbfa]: ((in_cache
  ((non-reproducible 1c8fc4744d4cef1bd2b8f5e915b36be9))) (computed
  ((non-reproducible 6cfaa7a90747882bcf4ffe7252c1cf89)))) after executing
  (echo 'build non-reproducible';cp dep non-reproducible)
  build reproducible
  build non-reproducible

  $ rm -rf _build
  $ DUNE_CACHE_CHECK_PROBABILITY=1.0 dune build --cache-check-probability=0.0 --cache=enabled reproducible non-reproducible

  $ rm -rf _build
  $ dune build --cache=enabled --cache-check-probability=1.0 reproducible non-reproducible
  Warning: cache store error [e4096cb8e17c59cb28c421878c60fbfa]: ((in_cache
  ((non-reproducible 1c8fc4744d4cef1bd2b8f5e915b36be9))) (computed
  ((non-reproducible 6cfaa7a90747882bcf4ffe7252c1cf89)))) after executing
  (echo 'build non-reproducible';cp dep non-reproducible)
  build reproducible
  build non-reproducible

  $ dune build --cache=enabled --cache-check-probability=8 reproducible non-reproducible
  Error: The reproducibility check probability must be in the range [0, 1].
  [1]
