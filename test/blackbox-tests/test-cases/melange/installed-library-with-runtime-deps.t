Test simple interactions between melange.emit and copy_files

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.7)
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
  >  (melange.runtime_deps index.txt nested/hello.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
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

  $ dune install --display short --root lib --prefix $PWD/prefix
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/index.txt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/nested/hello.txt

  $ mkdir -p app/assets
  $ cat > app/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > app/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name app))
  > (using melange 0.1)
  > EOF
  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (libraries foo))
  > EOF

  $ cat > app/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ mkdir -p app/output
  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @mel --display=short
  Entering directory 'app'
          melc output/node_modules/foo/foo.js
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js
  Leaving directory 'app'

  $ ls app/_build/default/output/node_modules/foo
  foo.js
  index.txt
  nested
