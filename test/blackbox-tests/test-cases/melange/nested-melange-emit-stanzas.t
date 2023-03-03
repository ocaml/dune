Make sure an error is returned if trying to nest `melange.emit` stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF
  $ mkdir -p a/b/c
  $ cat > a/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_system commonjs))
  > EOF
  $ cat > a/b/c/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (module_system commonjs))
  > EOF

  $ dune build @mel
  File "a/b/c/dune", line 1, characters 0-70:
  1 | (melange.emit
  2 |  (target output)
  3 |  (alias mel)
  4 |  (module_system commonjs))
  Error: melange.emit stanzas cannot be nested
  - a/dune:1
  - a/b/c/dune:1
  Hint: Move the melange.emit stanza from a/b/c to at least the level of a
  [1]
