cinaps doesn't work with (include_subdirs unqualified)

  $ cat > dune-project <<EOF
  > (lang dune 2.3)
  > (using cinaps 1.0)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (executable (name test))
  > EOF

  $ mkdir sub
  $ cat >sub/dune <<EOF
  > (cinaps (files test.ml))
  > EOF
  $ cat >sub/test.ml <<EOF
  > (*$ print_endline "\nhello" *)
  > (*$*)
  > let x = 1
  > EOF

  $ dune runtest --diff-command diff 2>&1 | head -2
  Error: exception { exn =
      ("Directory creates generated directories inside its descendant source
