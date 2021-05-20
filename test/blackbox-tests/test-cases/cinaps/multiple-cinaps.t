Multiple cinaps stanzas in the same dune file

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using cinaps 1.0)
  > EOF

  $ cat > dune <<EOF
  > (cinaps (files *.ml))
  > (cinaps (files *.mli))
  > EOF

  $ touch x.ml x.mli

  $ dune runtest --diff-command diff 2>&1

  $ touch foo.ml
  $ cat > dune <<EOF
  > (cinaps (files foo.ml))
  > (cinaps (files *oo.ml))
  > EOF
  $ dune runtest --diff-command diff 2>&1
  Error: Multiple rules generated for
  _build/default/.cinaps.a7811055/cinaps.ml-gen:
  - dune:1
  - dune:2
  -> required by alias cinaps in dune:1
  -> required by alias runtest
  [1]
