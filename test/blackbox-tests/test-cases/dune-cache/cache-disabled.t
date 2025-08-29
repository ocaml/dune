Set up project and cache directory

  $ echo "(lang dune 3.20)" > dune-project
 
  $ cat > config <<EOF
  > (lang dune 3.20)
  > (cache enabled)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml << EOF
  > let f x y = x + y
  > EOF

  $ export DUNE_CACHE_ROOT=$(pwd)/dune_test_cache
  $ mkdir $DUNE_CACHE_ROOT

Initial build, populating cache

  $ dune build --config-file config
  $ wc -l _build/log 
  31 _build/log
  $ ls $DUNE_CACHE_ROOT
  files
  meta
  temp
  values

Second build, no-op as cache is warm

  $ dune clean --config-file config
  $ dune build --config-file config
  $ wc -l _build/log 
  17 _build/log

Build with --cache=disabled, which should be a no-op as _build is already populated. But unfortunately it is not.

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  22 _build/log

Second build with --cache=disabled, should be the same. Here we see it is really a no-op

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

  $ dune clean
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  22 _build/log
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

Run the tests with cache except user rules

  $ rm config
  $ rm -rf $DUNE_CACHE_ROOT
  $ mkdir $DUNE_CACHE_ROOT
  $ cat > config <<EOF
  > (lang dune 3.20)
  > (cache enabled-except-user-rules)
  > EOF

Initial build, populating shared cache

  $ dune build --config-file config
  $ wc -l _build/log 
  17 _build/log
  $ ls $DUNE_CACHE_ROOT
  files
  meta
  temp
  values

Second build, no-op as cache is warm

  $ dune clean --config-file config
  $ dune build --config-file config
  $ wc -l _build/log 
  22 _build/log

Build with --cache=disabled, which should be a no-op as _build is already populated

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

Second build with --cache=disabled, should be the same

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

  $ dune clean
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  22 _build/log
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

Run the tests without cache 

  $ rm config
  $ rm -rf $DUNE_CACHE_ROOT
  $ mkdir $DUNE_CACHE_ROOT
  $ cat > config <<EOF
  > (lang dune 3.20)
  > (cache enabled-except-user-rules)
  > EOF

Initial build, populating local cache

  $ dune build --config-file config
  $ wc -l _build/log 
  17 _build/log
  $ ls $DUNE_CACHE_ROOT
  files
  meta
  temp
  values

Second build, no-op as cache is warm

  $ dune clean --config-file config
  $ dune build --config-file config
  $ wc -l _build/log 
  22 _build/log

Build with --cache=disabled, which should be a no-op as _build is already populated

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

Second build with --cache=disabled, should be the same

  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

  $ dune clean
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  22 _build/log
  $ dune build --cache=disabled --config-file config
  $ wc -l _build/log 
  17 _build/log

