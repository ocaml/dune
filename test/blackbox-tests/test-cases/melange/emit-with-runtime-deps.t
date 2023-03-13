Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (runtime_deps assets/file.txt (glob_files_rec ./globbed/*.txt)))
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF
  $ mkdir globbed
  $ echo a.txt > globbed/a.txt
  $ echo b.txt > globbed/b.txt

  $ cat > main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ dune build output/assets/file.txt --display=short
  $ find _build/default/output
  _build/default/output
  _build/default/output/assets
  _build/default/output/assets/file.txt
  $ dune clean

Not attached to alias because `output/` doesn't exist in the source dir

  $ dune rules @mel | grep file.txt
  [1]

Creating the source directory makes it appear in the alias

  $ mkdir output
  $ dune rules @mel | grep file.txt
  ((deps ((File (In_build_dir _build/default/assets/file.txt))))
   (targets ((files (default/output/assets/file.txt)) (directories ())))
   (action (chdir _build/default (copy assets/file.txt output/assets/file.txt))))

  $ dune build @mel --display=short
          melc .output.mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc output/main.js

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/
  assets
  globbed
  main.ml
  output
  $ ls _build/default/output
  assets
  globbed
  main.js

  $ dune build output/assets/file.txt --display=short
  $ ls _build/default/output
  assets
  globbed
  main.js
  $ ls _build/default/output/globbed
  a.txt
  b.txt


  $ node _build/default/output/main.js
  hello from file
  



