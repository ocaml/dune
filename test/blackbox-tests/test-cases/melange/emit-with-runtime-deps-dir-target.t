Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (system "mkdir %{target}")
  >    (system "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps ./some_dir))
  > EOF

  $ cat > main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./some_dir/inside-dir-target.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ dune build @mel
  Error: Is a directory
  -> required by _build/default/output/some_dir
  -> required by alias output/mel
  [1]
