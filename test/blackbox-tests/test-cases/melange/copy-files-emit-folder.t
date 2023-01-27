Showcase issue with melange.emit inside folder and copy_files

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir src

  $ cat > src/dune <<EOF
  > (melange.emit
  >  (target app)
  >  (alias melange)
  >  (module_system commonjs))
  > 
  > (subdir
  >  app
  >  (copy_files
  >   (files %{project_root}/assets/file.txt))
  >  (alias
  >   (name melange)
  >   (deps file.txt)))
  > EOF

  $ cat > src/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "../file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ dune build @melange
  $ node _build/default/src/app/src/main.js
  hello from file
  
Now add include_subdirs unqualified to show issue

  $ cat > src/dune <<EOF
  > (include_subdirs unqualified)
  > 
  > (melange.emit
  >  (target app)
  >  (alias melange)
  >  (module_system commonjs))
  > 
  > (subdir
  >  app
  >  (copy_files
  >   (files %{project_root}/assets/file.txt))
  >  (alias
  >   (name melange)
  >   (deps file.txt)))
  > EOF

  $ dune build @melange
  $ node _build/default/src/app/src/main.js 2>&1 | grep "no such file or directory"
  Error: ENOENT: no such file or directory, open '$TESTCASE_ROOT/_build/default/src/app/src/../file.txt'
