Test simple interactions between melange.emit and copy_files

  $ mkdir a
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > (using directory-targets 0.1)
  > EOF

  $ cat > a/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps assets/file.txt assets/file.txt))
  > EOF

  $ mkdir a/assets
  $ cat > a/assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > a/main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ dune rules @mel | grep file.txt
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
    ((files (_build/default/a/output/a/assets/file.txt)) (directories ())))
    (chdir _build/default (copy a/assets/file.txt a/output/a/assets/file.txt))))

  $ dune build @mel

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

  $ mkdir another
  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias non-existing-mel)
  >  (target another-output)
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps doesnt-exist.txt))
  > EOF

  $ dune build @non-existing-mel
  File "another/dune", lines 1-7, characters 0-177:
  1 | (melange.emit
  2 |  (alias non-existing-mel)
  3 |  (target another-output)
  4 |  (emit_stdlib false)
  5 |  (libraries melange.node)
  6 |  (preprocess (pps melange.ppx))
  7 |  (runtime_deps doesnt-exist.txt))
  Error: No rule found for another/doesnt-exist.txt
  [1]

Test depend on non-file dependencies

  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias non-existing-mel)
  >  (target another-output)
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps (sandbox none)))
  > EOF
  $ dune build @non-existing-mel
  File "another/dune", line 7, characters 15-29:
  7 |  (runtime_deps (sandbox none)))
                     ^^^^^^^^^^^^^^
  Error: only files are allowed in this position
  [1]

Test depending on paths that "escape" the melange.emit directory

  $ dune clean
  $ cat > another/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target another-output)
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps ../a/assets/file.txt))
  > EOF
  $ cat > another/main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Need to create the source dir first for the alias to be picked up

  $ dune rules @mel | grep .txt
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
    ((files (_build/default/a/output/a/assets/file.txt)) (directories ())))
    (chdir _build/default (copy a/assets/file.txt a/output/a/assets/file.txt))))
  ((deps ((File (In_build_dir _build/default/a/assets/file.txt))))
    ((files (_build/default/another/another-output/a/assets/file.txt))
     (copy a/assets/file.txt another/another-output/a/assets/file.txt))))

  $ dune build @mel

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
  >  (emit_stdlib false)
  >  (libraries melange.node)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps /etc/hosts))
  > EOF
  $ cat > external/main.ml <<EOF
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

  $ dune build @mel --display=short 2>&1 | grep -i main
           ppx external/main.pp.ml
          melc external/.external-output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc external/external-output/external/main.js

External paths are not copied to the target directory

  $ ls _build/default/external/external-output/external
  main.js

