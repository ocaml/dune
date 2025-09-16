Testing the passing of "-g" to the C compiler.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs
  >   (language c)
  >   (names stub)))
  > EOF

  $ cat > stub.c <<EOF
  > int mol = 42;
  > EOF

  $ cat > foo.ml

  $ dune build
  $ grep -e stub.c _build/log | grep -ce "-g "
  1

Building in release mode does not currently disable the debug flag.

  $ dune clean
  $ dune build --profile=release
  $ grep -e stub.c _build/log | grep -ce "-g "
  1

We are able to explicitly disable the debug flag by editing our enviornment.

  $ cat >> dune <<EOF
  > (env
  >  (_
  >   (c_flags :standard \ -g)))
  > EOF

  $ dune clean
  $ dune build
  $ grep -e stub.c _build/log | grep -ce "-g "
  0
  [1]

