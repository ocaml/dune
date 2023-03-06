Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > (using directory-targets 0.1)
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
  >  (runtime_deps ./some_dir))
  > EOF

  $ cat > main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./some_dir/inside-dir-target.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ mkdir output
  $ dune rules @mel | grep file.txt
  [1]

  $ dune build @mel --display=short
            sh some_dir
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
            sh some_dir
          melc output/main.js
  Error: Is a directory
  -> required by _build/default/output/some_dir
  -> required by alias output/mel
  [1]
