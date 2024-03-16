`enabled_if` in `melange.emit`

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (enabled_if %{bin-available:melc}))
  > EOF
  $ cat > x.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune rules @melange | grep '\.cmj'
     (File (In_build_dir _build/default/.out.mobjs/melange/melange__X.cmj))))
      .out.mobjs/melange/melange__X.cmj))))
  $ dune build @melange

  $ dune clean

`(enabled_if false)` shouldn't build any JS

  $ cat > dune <<EOF
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (enabled_if false))
  > EOF

  $ dune build @melange
  Error: Alias "melange" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

No rules attached to the alias

  $ dune rules @melange
  Error: Alias "melange" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]
