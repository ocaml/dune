  $ echo "(lang dune 3.1)" > dune-project
  $ cat >dune <<EOF
  > (executable
  >   (name c)
  >   (libraries (select c.ml from (!bigarray -> c.nobigarray.ml) (bigarray -> c.bigarray.ml) ( -> c.dummy.ml))))
  > EOF
  $ touch c.bigarray.ml  c.dummy.ml  c.nobigarray.ml
  $ dune build
