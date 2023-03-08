Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule (target (dir some_dir))
  >  (action
  >   (progn (bash "mkdir %{target}")
  >    (bash "echo hello from file inside dir target > %{target}/inside-dir-target.txt"))))
  > (melange.emit
  >  (alias mel)
  >  (target output)
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
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js
  File "dune", line 5, characters 0-71:
  5 | (melange.emit
  6 |  (alias mel)
  7 |  (target output)
  8 |  (runtime_deps ./some_dir))
  Error: Error trying to read targets after a rule was run:
  - output/some_dir: Unexpected file kind "S_DIR" (directory)
  [1]
