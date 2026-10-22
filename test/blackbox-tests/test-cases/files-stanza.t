The ``files`` stanza lets us ignore source artifacts produced by other build
tools.

  $ make_dune_project 3.21

First, without the stanza, a pre-existing artifact conflicts with an explicit
rule targeting the same filename.

  $ touch mymod.ml
  $ cat >dune <<EOF
  > (library
  >  (name mylib)
  >  (wrapped false))
  > (rule (with-stdout-to foo.xyz (progn)))
  > EOF
  $ touch mylib.cma
  $ dune build
  Error: Multiple rules generated for _build/default/mylib.cma:
  - dune:1
  - file present in source tree
  Hint: rm -f mylib.cma
  [1]
  $ rm mylib.cma
  $ touch foo.xyz
  $ dune build
  Error: Multiple rules generated for _build/default/foo.xyz:
  - dune:4
  - file present in source tree
  Hint: rm -f foo.xyz
  [1]

With ``(files ...)`` both source artifacts are ignored and the rules can build
the targets.

  $ touch mylib.cma
  $ cat >>dune <<EOF
  > (files :standard \ *.cma *.xyz)
  > EOF

  $ dune build
  $ ls _build/default
  foo.xyz
  mylib.a
  mylib.cma
  mylib.cmxa
  mylib.cmxs
  mymod.ml
