Make sure an error is returned if trying to nest `melange.emit` stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF
  $ mkdir -p a/output/b
  $ cat > a/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_systems commonjs))
  > EOF
  $ cat > a/output/b/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_systems commonjs))
  > EOF

  $ dune build @mel
  File "a/output/b/dune", line 1, characters 0-71:
  1 | (melange.emit
  2 |  (target output)
  3 |  (alias mel)
  4 |  (module_systems commonjs))
  Error: melange.emit stanzas cannot be nested
  - a/dune:1
  - a/output/b/dune:1
  Hint: Move the melange.emit stanza from a/output/b to at least the level of a
  [1]
