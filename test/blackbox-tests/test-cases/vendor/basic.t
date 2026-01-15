Test basic vendor stanza functionality

  $ cat > dune-project << EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p mylib.1.0.0

Create a vendored library (directly in mylib.1.0.0/):
  $ cat > mylib.1.0.0/dune-project << EOF
  > (lang dune 3.22)
  > (name mylib)
  > (package (name mylib))
  > EOF

  $ cat > mylib.1.0.0/dune << EOF
  > (library
  >  (name mylib)
  >  (public_name mylib))
  > EOF

  $ cat > mylib.1.0.0/mylib.ml << EOF
  > let greeting = "Hello from vendored mylib"
  > EOF

Create dune file with vendor stanza:
  $ cat > dune << EOF
  > (vendor mylib.1.0.0)
  > (executable
  >  (name main)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml << EOF
  > let () = print_endline Mylib.greeting
  > EOF

Build and run:
  $ dune exec ./main.exe
  Hello from vendored mylib
