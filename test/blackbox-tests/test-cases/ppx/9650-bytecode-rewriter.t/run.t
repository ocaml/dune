  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

  $ mkdir bin
  $ cat > bin/dune << EOF
  > (executable
  >  (name e)
  >  (preprocess (pps fooppx)))
  > EOF
  $ cat >bin/e.ml <<EOF
  > print_endline "Hello World"
  > EOF

  $ dune exec -- bin/e.exe
  Hello World
