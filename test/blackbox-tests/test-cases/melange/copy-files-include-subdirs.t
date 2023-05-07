Example using melange.emit, copy_files and include_subdirs

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ mkdir src

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target app)
  >  (preprocess (pps melange.ppx))
  >  (emit_stdlib false)
  >  (alias mel))
  > 
  > (subdir
  >  app
  >  (copy_files
  >   (files %{project_root}/assets/file.txt))
  >  (alias
  >   (name mel)
  >   (deps file.txt)))
  > EOF

  $ cat > src/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "../file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ output_dir=_build/default/src/app
  $ src=$output_dir/src/main.js
  $ asset=$output_dir/file.txt
  $ dune build @mel
  $ dune build $asset
  $ node $src
  hello from file
  
Now add include_subdirs unqualified to show issue

  $ echo "(include_subdirs unqualified)" >> src/dune

  $ dune build @mel
  $ dune build $asset
  $ node $src
  hello from file
  
