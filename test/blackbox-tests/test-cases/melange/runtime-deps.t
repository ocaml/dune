Test runtime_deps field

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (libraries lib)
  >  (module_system commonjs))
  > 
  > (library
  >  (name lib)
  >  (modules lib)
  >  (modes melange)
  >  (melange.runtime_deps assets/file.txt))
  > EOF

  $ cat > lib.ml <<EOF
  > let file_path = "assets/file.txt"
  > let file_content = Node.Fs.readFileSync file_path \`utf8
  > let print_file () = Js.log file_content
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > main.ml <<EOF
  > let () = Lib.print_file ()
  > EOF

  $ dune build output/main.js

  $ node _build/default/output/main.js
  hello from file
  
