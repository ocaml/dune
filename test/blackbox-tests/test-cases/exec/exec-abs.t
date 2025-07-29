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

  $ dune exec $PWD/foo.exe
  Error: Program
  '$TESTCASE_ROOT/foo.exe'
  not found!
  [1]

