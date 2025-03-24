enabled_if and build_if have similar behavior on the test(s) stanza.
build_if controls whether the tests builds, while enabled_if controls whether
the test runs as part of @runtest


  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > EOF

  $ cat > dune << EOF
  > (test
  >  (name t)
  >  (enabled_if %{env:ENABLED=false}))
  > EOF

  $ touch t.ml

We test the various combinations:

  $ test_one () {
  >   dune clean
  >   output=$( dune build "$1" --display short 2>&1 )
  >   echo When building $1 with ENABLED=${ENABLED:-unset}:
  >   if echo $output|grep -q ocamlopt ; then
  >     echo '  build was done: YES'
  >   else
  >     echo '  build was done: NO'
  >   fi
  >   if echo $output|grep -q "alias runtest" ; then
  >     echo '  test did run:   YES'
  >   else
  >     echo '  test did run:   NO'
  >   fi
  > }

  $ test_all () {
  >   test_one @all
  >   test_one @runtest
  >   ENABLED=true test_one @all
  >   ENABLED=true test_one @runtest
  > }

  $ test_all
  When building @all with ENABLED=unset:
    build was done: YES
    test did run:   NO
  When building @runtest with ENABLED=unset:
    build was done: NO
    test did run:   NO
  When building @all with ENABLED=true:
    build was done: YES
    test did run:   NO
  When building @runtest with ENABLED=true:
    build was done: YES
    test did run:   YES

Now with build_if:

  $ cat > dune << EOF
  > (test
  >  (name t)
  >  (build_if %{env:ENABLED=false}))
  > EOF

Notice that in the first case, nothing is done at all:

  $ test_all
  When building @all with ENABLED=unset:
    build was done: NO
    test did run:   NO
  When building @runtest with ENABLED=unset:
    build was done: NO
    test did run:   NO
  When building @all with ENABLED=true:
    build was done: YES
    test did run:   NO
  When building @runtest with ENABLED=true:
    build was done: YES
    test did run:   YES
