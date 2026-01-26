Testing the coqpp stanza

  $ cat > dune << EOF
  > (coq.pp
  >  (modules gram))
  > (library
  >  (name foo)
  >  (flags -rectypes)
  >  (libraries coq-core.vernac))
  > EOF

  $ ls
  dune
  dune-project
  gram.mlg
  marg.mlg

  $ dune build

  $ ls _build/default/
  foo.a
  foo.cma
  foo.cmxa
  foo.cmxs
  foo.ml-gen
  gram.ml
  gram.mlg

Testing the :standard field with a .mlg file depending on another .mlg file
  $ cat > dune << EOF
  > (coq.pp
  >  (modules :standard))
  > (library
  >  (name foo)
  >  (flags -rectypes)
  >  (libraries coq-core.vernac))
  > EOF

  $ ls
  _build
  dune
  dune-project
  gram.mlg
  marg.mlg

  $ dune build

  $ ls _build/default/
  foo.a
  foo.cma
  foo.cmxa
  foo.cmxs
  foo.ml-gen
  gram.ml
  gram.mlg
  marg.ml
  marg.mlg

Same again but with one .mlg file removed
  $ cat > dune << EOF
  > (coq.pp
  >  (modules :standard \ gram))
  > (library
  >  (name foo)
  >  (flags -rectypes)
  >  (libraries coq-core.vernac))
  > EOF

  $ dune build
  File "marg.mlg", line 3, characters 12-20:
  Error: Unbound module Gram
  [1]

  $ ls _build/default/
  foo.a
  foo.cma
  foo.cmxa
  foo.cmxs
  foo.ml-gen
  gram.mlg
  marg.ml
  marg.mlg
