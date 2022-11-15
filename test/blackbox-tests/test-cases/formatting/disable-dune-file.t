This tests how it is possible to disable formatting for a particular dialect in
a given subdirectory. This can be used to disable formatting of a particular
dune file.

  $ cat > dune-project << EOF
  > (lang dune 2.8)
  > EOF

  $ cat > dune << EOF
  > ; this file should be formatted
  > (rule (write-file a b))
  > EOF

  $ mkdir sub
  $ cat > sub/dune << EOF
  > ; this file should not
  > (env
  >  (_
  >   (formatting (enabled_for ocaml))))
  > 
  > (rule (write-file a b))
  > EOF

  $ dune build @fmt
  File "dune", line 1, characters 0-0:
  Error: Files _build/default/dune and _build/default/.formatted/dune differ.
  [1]
