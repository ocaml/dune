Instrumentation libraries on `melange.emit` are only added when the backend is
active.

  $ make_melange_project 3.25 1.0

  $ mkdir ppx helper
  $ cat >dune <<'EOF'
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main)
  >  (instrumentation
  >   (backend noop)
  >   (libraries helper)))
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Js.log Helper.message
  > EOF

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name noop_ppx)
  >  (kind ppx_rewriter)
  >  (modules noop_ppx)
  >  (libraries ppxlib))
  > 
  > (library
  >  (name noop)
  >  (modules noop)
  >  (instrumentation.backend
  >   (ppx noop_ppx)))
  > EOF
  $ cat >ppx/noop_ppx.ml <<'EOF'
  > let () = Ppxlib.Driver.register_transformation "noop"
  > EOF
  $ cat >ppx/noop.ml <<'EOF'
  > EOF

  $ cat >helper/dune <<'EOF'
  > (library
  >  (name helper)
  >  (modes melange)
  >  (modules helper))
  > EOF
  $ cat >helper/helper.ml <<'EOF'
  > let message = "instrumentation library"
  > EOF

Without instrumentation, the library is ignored.

  $ dune build @melange
  File ".melange_src/main.ml", line 1, characters 16-22:
  1 | let () = Js.log Helper.message
                      ^^^^^^
  Error: Unbound module Helper
  [1]

With instrumentation enabled, the library is added to the dependencies.

  $ dune build --instrument-with noop @melange
  $ node _build/default/output/main.js
  instrumentation library
