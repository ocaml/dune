Entry points should not allow mli only modules as entry points.

  $ make_melange_project 3.8 0.1

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
