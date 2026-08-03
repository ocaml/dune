Show target promotion in-source for `melange.emit`

  $ make_melange_project 3.20 1.0

  $ mkdir -p app
  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries melange.dom)
  >  (promote (until-clean))
  >  (target dist))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = Js.log "hello"
  > EOF
  $ dune build @dist

Targets get emitted in source

  $ ls app
  dune
  x.js
  x.ml

  $ ls node_modules/melange.dom
  dom.js
  dom__.js
  dom_storage.js
