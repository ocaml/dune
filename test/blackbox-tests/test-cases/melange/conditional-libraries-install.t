Exercise installed library metadata for native and Melange-specific
dependencies.

  $ mkdir -p lib_for_melange lib_for_native app/lib

  $ cat > lib_for_melange/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name lib_for_melange))
  > (using melange 0.1)
  > EOF
  $ cat > lib_for_melange/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name lib_for_melange))
  > EOF
  $ cat > lib_for_melange/foo.ml <<EOF
  > let x = "lib for melange"
  > EOF

  $ dune build --root lib_for_melange
  $ dune install --root lib_for_melange --prefix $PWD/prefix

  $ cat > lib_for_native/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name lib_for_native))
  > (using melange 0.1)
  > EOF
  $ cat > lib_for_native/dune <<EOF
  > (library
  >  (modes :standard)
  >  (public_name lib_for_native))
  > EOF
  $ cat > lib_for_native/foo.ml <<EOF
  > let x = "lib for native"
  > EOF

  $ dune build --root lib_for_native
  $ dune install --root lib_for_native --prefix $PWD/prefix

  $ cat > app/dune-project <<EOF
  > (lang dune 3.24)
  > (package (name app))
  > (using melange 0.1)
  > EOF

  $ cat > app/lib/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name lib_for_app)
  >  (libraries lib_for_native)
  >  (melange.libraries lib_for_melange)
  >  (melange.preprocess (pps melange.ppx))
  >  (public_name app))
  > EOF

  $ cat > app/lib/common_intf.mli <<EOF
  > val print : string -> unit
  > EOF
  $ cat > app/lib/common_intf.ml <<EOF
  > let message = Lib_for_native.Foo.x
  > let print prefix = Format.eprintf "%s%s@." prefix message
  > EOF
  $ cat > app/lib/common_intf.melange.ml <<EOF
  > let message = Lib_for_melange.Foo.x
  > let print prefix = Js.log2 prefix message
  > EOF
  $ cat > app/lib/lib_for_app.ml <<EOF
  > let say_hello () = Common_intf.print "message: "
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app
  $ dune install --root app --prefix $PWD/prefix
  $ cat prefix/lib/app/dune-package | grep requires
   (requires lib_for_native)
   (melange_requires lib_for_melange)

Now use the installed common library for an executable and a `melange.emit`

  $ mkdir use-installed
  $ cat > use-installed/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 0.1)
  > EOF

  $ cat > use-installed/dune <<EOF
  > (melange.emit
  >  (target out)
  >  (modules x)
  >  (emit_stdlib false)
  >  (libraries app))
  > 
  > (executable
  >  (name x)
  >  (modules x)
  >  (libraries app))
  > EOF
  $ cat > use-installed/x.ml <<EOF
  > let () = Lib_for_app.say_hello ()
  > EOF

  $ cd use-installed
  $ OCAMLPATH=$PWD/../prefix/lib/:$OCAMLPATH dune build @all

  $ node ./_build/default/out/x.js
  message:  lib for melange

  $ ./_build/default/x.exe
  message: lib for native
