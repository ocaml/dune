Test `melange.runtime_deps` in a private library

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using directory-targets 0.1)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (libraries foo)
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps assets/file.txt))
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (library
  >  (name foo)
  >  (modes melange)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps ./some_dir))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./some_dir/inside-dir-target.txt"
  > let read_asset () = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > main.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > let () = Js.log file_content
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ dune build @mel

The directory target runtime_dep some_dir was copied to the library build folder

  $ ls _build/default/lib
  foo.ml
  foo.pp.ml
  some_dir

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/output
  assets
  lib
  main.js

  $ ls _build/default/output/lib
  foo.js
  some_dir
  $ node _build/default/output/main.js
  hello from file
  
  hello from file inside dir target
  
