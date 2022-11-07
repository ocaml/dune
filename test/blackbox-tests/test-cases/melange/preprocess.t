Test (preprocess) field on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (entries main)
  >  (module_system commonjs)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build output/melange__Main.js
  $ node _build/default/output/melange__Main.js
  hello
