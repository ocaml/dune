Testing the coq.ffi stanza with local libraries

  $ cat > dune << EOF
  > (coq.ffi
  >  (modules hello)
  >  (library hello))
  > 
  > (library
  >  (name hello)
  >  (modules hello))
  > EOF

  $ dune build
  fake coqffi has run with args:
  .hello.objs/byte/hello.cmi
  -o
  hello.v
  
  $ ls _build/default/
  hello.a
  hello.cma
  hello.cmxa
  hello.cmxs
  hello.ml
  hello.mli
  hello.v

The coq.ffi stanza does not support libraries that were not installed using Dune

  $ cat > dune << EOF
  > (coq.ffi
  >  (modules unix)
  >  (library unix))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-41:
  1 | (coq.ffi
  2 |  (modules unix)
  3 |  (library unix))
  Error: Library "unix" was not installed using Dune and therefore not
  supported by coq.ffi.
  [1]
