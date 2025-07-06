Building a project with 2 melange.emit stanzas should add rules to both aliases

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
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

  $ dune build @mel --display=short
  $ dune build @second --display=short
