Test `melange.runtime_deps` for installed libraries where the dune file is
nested more than the dune-project file

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir -p lib/packages/foo/src/
  $ echo "function foo() { return 42; }" > lib/packages/foo/src/runtime.js
  $ cat > lib/packages/foo/src/dune <<EOF
  > (library
  >  (public_name foo)
  >  (name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./runtime.js))
  > EOF
  $ cat > lib/packages/foo/src/foo.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/runtime.js") \`utf8
  > EOF

  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/runtime.js"
  ]

  $ cat lib/_build/install/default/lib/foo/dune-package | grep melange_runtime_deps
   (melange_runtime_deps runtime.js))

  $ dune install --root lib --prefix $PWD/prefix

  $ mkdir -p app
  $ cat > app/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name app))
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries foo))
  > EOF

  $ cat > app/main.ml <<EOF
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @melange
  Entering directory 'app'
  Leaving directory 'app'

File exists in output dir

  $ ls app/_build/default/output/node_modules/foo
  foo.js
  runtime.js
