Test that the shared cache is hit when the build directory is named by its
absolute path (e.g. DUNE_BUILD_DIR=$PWD/_build) rather than the default
relative name ("_build").

  $ export DUNE_CACHE_ROOT=$PWD/.cache
  $ export DUNE_CACHE=enabled
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (deps source)
  >  (targets target)
  >  (action (system "cat source > target")))
  > EOF
  $ echo hello > source

First build: populates the shared cache (cache miss expected).

  $ dune build --display=short target 2>&1
            sh target

Second build with _build removed: shared cache hit, so the action is not
re-run and there is no output.

  $ rm -rf _build
  $ dune build --display=short target 2>&1

Third build with an absolute build directory path that resolves to the same
directory: should also be a shared cache hit.

  $ rm -rf _build
  $ DUNE_BUILD_DIR=$PWD/_build dune build --display=short target 2>&1
