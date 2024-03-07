Test `melange.runtime_deps` in a library that has been installed

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ mkdir -p lib/nested
  $ echo "Some text" > lib/index.txt
  $ echo "Some nested text" > lib/nested/hello.txt
  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps index.txt nested/hello.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let file_path = "./index.txt"
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > EOF

  $ dune build --root lib
  Entering directory 'lib'
  Leaving directory 'lib'

  $ cat lib/_build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/index.txt"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/nested/hello.txt" {"nested/hello.txt"}
  ]

  $ cat lib/_build/install/default/lib/foo/dune-package | grep melange_runtime_deps
   (melange_runtime_deps index.txt nested/hello.txt))

  $ dune install --root lib --prefix $PWD/prefix

  $ mkdir -p app/assets
  $ cat > app/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > app/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name app))
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (libraries foo melange.node)
  >  (preprocess (pps melange.ppx)))
  > EOF

  $ cat > app/main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @mel
  Entering directory 'app'
  Leaving directory 'app'

  $ ls app/_build/default/output/node_modules/foo
  foo.js
  index.txt
  nested
