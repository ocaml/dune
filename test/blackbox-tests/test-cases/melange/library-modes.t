Test library modes

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

Build a regular library and an executable

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib))
  > EOF

  $ cat > lib/mylib.ml <<EOF
  > let some_binding = "string"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modes byte exe)
  >  (libraries mylib))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline Mylib.some_binding
  > EOF

  $ dune build main.bc

Now let's make the library compatible with melange

  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib)
  >  (modes :standard melange))
  > EOF

  $ cat > main_melange.ml <<EOF
  > let () =
  >   print_endline Mylib.some_binding
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modes byte exe)
  >  (modules main)
  >  (libraries mylib))
  > (melange.emit
  >  (alias dist)
  >  (modules main_melange)
  >  (emit_stdlib false)
  >  (target dist)
  >  (libraries mylib))
  > EOF

  $ dune build @dist

  $ dune build main.bc

