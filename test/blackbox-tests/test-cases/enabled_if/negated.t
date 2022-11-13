For dune >= 3.2, negating expressions is allowed
  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > EOF
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
