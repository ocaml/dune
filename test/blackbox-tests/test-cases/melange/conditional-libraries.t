`melange.libraries` applies only to Melange compilation.

The field is available starting in Dune 3.24.

  $ make_old_melange_field_project melange.libraries dep
  $ dune build --root old
  Entering directory 'old'
  File "dune", line 4, characters 1-24:
  4 |  (melange.libraries dep))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'melange.libraries' is only available since version 3.24 of the dune
  language. Please update your dune-project file to have (lang dune 3.24).
  Leaving directory 'old'
  [1]
  $ rm -rf old

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF

  $ mkdir lib_for_melange lib_for_native app
  $ cat > lib_for_melange/dune <<EOF
  > (library
  >  (modes melange)
  >  (name lib_for_melange))
  > EOF
  $ cat > lib_for_melange/foo.ml <<EOF
  > let x = "lib for melange"
  > EOF

  $ cat > lib_for_native/dune <<EOF
  > (library
  >  (modes :standard)
  >  (name lib_for_native))
  > EOF
  $ cat > lib_for_native/foo.ml <<EOF
  > let x = "lib for native"
  > EOF

  $ cat > app/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name app)
  >  (modules app common_intf)
  >  (libraries lib_for_native)
  >  (melange.libraries lib_for_melange))
  > 
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (emit_stdlib false)
  >  (libraries app))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries app))
  > EOF

  $ cat > app/common_intf.mli <<EOF
  > val print : string -> unit
  > EOF
  $ cat > app/common_intf.ml <<EOF
  > let message = Lib_for_native.Foo.x
  > let print prefix = Format.eprintf "%s%s@." prefix message
  > EOF
  $ cat > app/common_intf.melange.ml <<EOF
  > let message = Lib_for_melange.Foo.x
  > let print prefix = Js.log2 prefix message
  > EOF
  $ cat > app/app.ml <<EOF
  > let say_hello () = Common_intf.print "message: "
  > EOF
  $ cat > app/main.ml <<EOF
  > let () = App.say_hello ()
  > EOF

  $ dune build @app/melange app/main.exe
  $ node _build/default/app/out/app/main.js
  message:  lib for melange
  $ _build/default/app/main.exe
  message: lib for native

The field is rejected without Melange mode.

  $ mkdir no-melange
  $ cat > no-melange/dune <<EOF
  > (library
  >  (modes :standard)
  >  (name no_melange)
  >  (melange.libraries lib_for_melange))
  > EOF
  $ dune build no-melange
  File "no-melange/dune", lines 1-4, characters 0-84:
  1 | (library
  2 |  (modes :standard)
  3 |  (name no_melange)
  4 |  (melange.libraries lib_for_melange))
  Error: Cannot specify `melange.libraries' without `melange' mode
  [1]
