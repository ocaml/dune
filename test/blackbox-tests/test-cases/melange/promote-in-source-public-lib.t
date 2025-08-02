Show target promotion in-source for `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > (using melange 1.0)
  > EOF

  $ mkdir -p app
  $ cat > app/dune <<EOF
  > (include_subdirs unqualified)
  > (melange.emit
  >  (alias dist)
  >  (promote (until-clean))
  >  (target dist))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = print_endline "hello"
  > EOF
  $ dune build @dist

Targets get emitted in source

  $ ls app
  dune
  x.js
  x.ml

  $ ls node_modules/melange | grep list
  list.js
  listLabels.js
