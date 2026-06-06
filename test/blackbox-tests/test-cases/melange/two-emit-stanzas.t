Building a project with 2 melange.emit stanzas should add rules to both aliases

  $ make_melange_project 3.8 0.1
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias mel)
  >  (promote (until-clean))
  >  (modules)
  >  (emit_stdlib false)
  >  (module_systems
  >   (commonjs js)))
  > 
  > (melange.emit
  >  (target dist-es6)
  >  (alias second)
  >  (promote (until-clean))
  >  (emit_stdlib false)
  >  (modules)
  >  (module_systems
  >   (es6 mjs)))
  > EOF

  $ dune build @mel
  $ dune build @second
