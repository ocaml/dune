Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (runtime_deps assets/file.txt assets/file.txt)
  >  (module_system commonjs))
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ mkdir output
  $ dune rules @mel | grep file.txt
  ((deps ((File (In_build_dir _build/default/assets/file.txt))))
   (targets ((files (default/output/assets/file.txt)) (directories ())))
     (symlink ../../assets/file.txt output/assets/file.txt))))

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/output/assets
  file.txt

  $ dune build output/assets/file.txt --display=short
  $ ls _build/default/output
  assets
  main.js


  $ node _build/default/output/main.js
  hello from file
  


Test depending on non-existing paths

  $ mkdir another
  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias non-existing-mel)
  >  (target another-output)
  >  (runtime_deps doesnt-exist.txt)
  >  (module_system commonjs))
  > EOF

  $ dune build @non-existing-mel --display=short
  Error: No rule found for another/doesnt-exist.txt
  -> required by alias another/non-existing-mel
  [1]

Test depending on paths that "escape" the melange.emit directory

  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target another-output)
  >  (runtime_deps ../assets/file.txt)
  >  (module_system commonjs))
  > EOF
  $ cat > another/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Need to create the source dir first for the alias to be picked up

  $ mkdir -p another/another-output/assets
  $ dune rules @mel | grep .txt
  ((deps ((File (In_build_dir _build/default/assets/file.txt))))
    ((files (default/another/another-output/assets/file.txt)) (directories ())))
     (symlink ../../../assets/file.txt another/another-output/assets/file.txt))))
  ((deps ((File (In_build_dir _build/default/assets/file.txt))))
   (targets ((files (default/output/assets/file.txt)) (directories ())))
     (symlink ../../assets/file.txt output/assets/file.txt))))

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc another/.another-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js
          melc another/another-output/another/main.js

Path ends ups being emitted "correctly", but ouside the target dir.
  $ ls _build/default/another/another-output/another
  main.js
  $ ls _build/default/another/another-output/assets
  file.txt

