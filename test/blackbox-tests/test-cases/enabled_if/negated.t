For dune >= 3.2, negating expressions is allowed
  $ make_dune_project 3.2
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (enabled_if (not false)))
  > EOF
  $ cat > foo.ml <<EOF
  > print_endline "runs";;
  > EOF
  $ dune exec ./foo.exe
  runs
