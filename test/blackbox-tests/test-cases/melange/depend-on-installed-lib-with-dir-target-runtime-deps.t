Test `melange.runtime_deps` in a library that has been installed

  $ mkdir lib app prefix
  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ mkdir -p lib
  $ echo "Some text" > lib/index.txt
  $ cat > lib/dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./some_dir ./index.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let file_path = "./some_dir/inside-dir-target.txt"
  > let read_asset () = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
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
    "_build/install/default/lib/foo/some_dir/inside-dir-target.txt" {"some_dir/inside-dir-target.txt"}
  ]

  $ cat lib/_build/install/default/lib/foo/dune-package | grep melange_runtime_deps
   (melange_runtime_deps index.txt some_dir))

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
  >  (libraries foo)
  >  (preprocess (pps melange.ppx)))
  > EOF

  $ cat > app/main.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > let () = Js.log file_content
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @mel --debug-dependency-path
  Entering directory 'app'
  Leaving directory 'app'

  $ ls app/_build/default/output/node_modules/foo
  foo.js
  index.txt
  some_dir
