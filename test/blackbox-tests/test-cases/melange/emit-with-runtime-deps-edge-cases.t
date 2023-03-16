Test simple interactions between melange.emit and copy_files

  $ mkdir a
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > a/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (runtime_deps assets/file.txt assets/file.txt))
  > EOF

  $ mkdir a/assets
  $ cat > a/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > a/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ mkdir a/output
  $ dune rules @mel | grep file.txt
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
   (targets ((files (default/a/output/a/assets/file.txt)) (directories ())))
    (chdir _build/default (copy a/assets/file.txt a/output/a/assets/file.txt))))

  $ dune build @mel --display=short
          melc a/.output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc a/output/a/main.js

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/a/output/a/assets
  file.txt

  $ dune build a/output/a/assets/file.txt --display=short
  $ ls _build/default/a/output/a
  assets
  main.js


  $ node _build/default/a/output/a/main.js
  hello from file
  

Test depending on non-existing paths

  $ mkdir -p another/another-output/another
  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias non-existing-mel)
  >  (target another-output)
  >  (runtime_deps doesnt-exist.txt))
  > EOF

  $ dune build @non-existing-mel
  File "another/dune", line 1, characters 0-98:
  1 | (melange.emit
  2 |  (alias non-existing-mel)
  3 |  (target another-output)
  4 |  (runtime_deps doesnt-exist.txt))
  Error: No rule found for another/doesnt-exist.txt
  [1]

Test depending on paths that "escape" the melange.emit directory

  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target another-output)
  >  (runtime_deps ../a/assets/file.txt))
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
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
   (targets ((files (default/a/output/a/assets/file.txt)) (directories ())))
    (chdir _build/default (copy a/assets/file.txt a/output/a/assets/file.txt))))
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
    ((files (default/another/another-output/a/assets/file.txt))
     (copy a/assets/file.txt another/another-output/a/assets/file.txt))))

  $ dune build @mel --display=short
          melc a/.output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc another/.another-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc a/output/a/main.js
          melc another/another-output/another/main.js

Path ends ups being emitted "correctly", but outside the target dir.
  $ ls _build/default/another/another-output/another
  main.js
  $ ls _build/default/another/another-output/a/assets
  file.txt

Test depending on external paths

  $ mkdir external
  $ cat > external/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target external-output)
  >  (runtime_deps /etc/hosts))
  > EOF
  $ cat > external/main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ dune build @mel --display=short
          melc external/.external-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc external/external-output/external/main.js

External paths are not copied to the target directory

  $ ls _build/default/external/external-output/external
  main.js

