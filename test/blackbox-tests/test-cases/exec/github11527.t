Reproducing github #11527

When running dune exec on a binary `./bug.exe` we can expect that the working directory
and argv.(0) can be concatenated to get a valid path.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name bug))
  > EOF

  $ cat > bug.ml <<EOF
  > let () =
  >  Printf.printf "pwd: %s\n%!" (Sys.getcwd());
  >  Printf.printf "exe: %s\n%!" (Sys.argv.(0));
  > EOF

When running dune exec from the root this is true.
  $ dune exec -- ./bug.exe
  pwd: $TESTCASE_ROOT
  exe: ./_build/default/bug.exe

  $ mkdir subdir

As expected, the `exe:` argument shows the correct path
  $ (cd subdir && dune exec --root .. -- ./bug.exe)
  Entering directory '..'
  Leaving directory '..'
  pwd: $TESTCASE_ROOT/subdir
  exe: ../_build/default/bug.exe
 
