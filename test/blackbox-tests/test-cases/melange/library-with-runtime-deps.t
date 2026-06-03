Test `melange.runtime_deps` in a private library

  $ make_melange_runtime_deps_project

  $ mkdir lib
  $ echo "Some text" > lib/index.txt
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange)
  >  (preprocess (pps melange.ppx))
  >  (melange.runtime_deps index.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./index.txt"
  > let read_asset () = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > EOF

  $ write_melange_asset_reader

  $ dune build @mel

The runtime_dep index.txt was copied to the library build folder

  $ ls _build/default/lib
  foo.ml
  index.txt

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/output/lib
  foo.js
  index.txt
  $ node _build/default/output/main.js
  hello from file
  
  Some text
  
