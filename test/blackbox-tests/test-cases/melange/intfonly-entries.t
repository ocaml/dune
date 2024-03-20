Entry points should not allow mli only modules as entry points.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat >dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules_without_implementation foo)
  >  (alias mel))
  > EOF

  $ touch foo.mli bar.ml
  $ dune build @mel
  $ ls _build/default/output/*.js | sort
  _build/default/output/bar.js
