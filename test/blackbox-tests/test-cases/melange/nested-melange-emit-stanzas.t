Make sure an error is returned if trying to nest `melange.emit` stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  $ mkdir -p a/output/b
  $ cat > a/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (module_systems commonjs))
  > EOF
  $ cat > a/output/b/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (module_systems commonjs))
  > EOF

  $ dune build @mel
  File "a/output/b/dune", lines 1-5, characters 0-92:
  1 | (melange.emit
  2 |  (target output)
  3 |  (alias mel)
  4 |  (emit_stdlib false)
  5 |  (module_systems commonjs))
  Error: melange.emit stanzas cannot be nested
  - a/dune:1
  - a/output/b/dune:1
  Hint: Move both `melange.emit' stanzas to the same level.
  [1]
