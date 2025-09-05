Show that Dune shouldn't emit a JS file for the `melange.emit` module group
wrapper

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (using melange 0.1)
  > EOF

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

  $ dune build @dist --display=short 2>&1 | grep 'melange\.js'
  [1]
  $ ls -a _build/default/dist
  .
  ..
  x.js
  y.js

