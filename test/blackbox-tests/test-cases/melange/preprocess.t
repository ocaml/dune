Test (preprocess) field on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (alias melange)
  >  (module_system commonjs)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > output/main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @melange
  $ node _build/default/output/main.js
  hello
