Testing interaction of dune exec and absolute directories.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let () = print_endline "hi" ;;
  > EOF

  $ dune exec ./foo.exe
  hi

Dune exec is able to handle absolute executable paths.

  $ dune exec $PWD/foo.exe
  hi

Lets check some validation:

  $ dune exec ..
  Error: Program '..' not found!
  [1]
  $ dune exec .
  Error: Program '.' not found!
  [1]
  $ dune exec /.
  Error: Program '/.' not found!
  [1]
  $ dune exec /
  Error: Program '/' not found!
  [1]

