Testing the coq.ffi stanza with local libraries

  $ cat > dune << EOF
  > (coq.ffi
  >  (modules hello)
  >  (library hello))
  > 
  > (library
  >  (name hello)
  >  (modules hello))
  > 
  > (coq.theory
  >  (name hello))
  > EOF

  $ dune build
  fake coqffi has run with args:
  .hello.objs/byte/hello.cmi
  -o
  Hello.v
  
  Inductive coqffiHasRunAndVHasCompiled : Prop :=  .
  $ ls _build/default/
  Hello.glob
  Hello.v
  Hello.v.d
  Hello.vo
  Hello.vok
  Hello.vos
  hello.a
  hello.cma
  hello.cmxa
  hello.cmxs
  hello.ml
  hello.mli

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
  Error: Library "unix" was not installed using Dune and is therefore not
  supported by the coq.ffi stanza.
  [1]

Testing the coq.ffi stanza with non-existant modules

  $ cat > dune << EOF
  > (coq.ffi
  >  (modules foo)
  >  (library hello))
  > 
  > (library
  >  (name hello)
  >  (modules hello))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-41:
  1 | (coq.ffi
  2 |  (modules foo)
  3 |  (library hello))
  Error: Module "Foo" was not found in library "hello".
  [1]

Testing the coq.ffi stanza with flags

  $ cat > dune << EOF
  > (coq.ffi
  >  (modules hello)
  >  (library hello)
  >  (flags --some --flags))
  > 
  > (library
  >  (name hello)
  >  (modules hello))
  > EOF

  $ dune build
  fake coqffi has run with args:
  .hello.objs/byte/hello.cmi
  -o
  Hello.v
  --some
  --flags
  
