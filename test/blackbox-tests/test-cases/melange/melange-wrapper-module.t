Show that Dune shouldn't emit a JS file for the `melange.emit` module group
wrapper

  $ make_melange_project 3.18 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (target dist)
  >  (emit_stdlib false))
  > EOF

If there's more than 1 module, we currently emit the module group wrapper,
`.dist.mobjs/melange.js`

  $ cat > x.ml <<EOF
  > let () = print_endline "x"
  > EOF
  $ cat > y.ml <<EOF
  > let () = print_endline "y"
  > EOF

  $ dune build @dist
  $ find _build/default/dist -type f | sort
  _build/default/dist/x.js
  _build/default/dist/y.js

