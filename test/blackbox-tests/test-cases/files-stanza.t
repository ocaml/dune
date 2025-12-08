The ``files`` stanza lets us ignore source artifacts produced by other build
tools.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

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
  $ touch foo.xyz
  $ dune build
  Error: Multiple rules generated for _build/default/foo.xyz:
  - dune:4
  - file present in source tree
  Hint: rm -f foo.xyz
  [1]

With ``(files ...)`` the source artifact is ignored and the rule can build the
target.

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
