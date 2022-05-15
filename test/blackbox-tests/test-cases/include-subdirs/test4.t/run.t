Test for (include_subdir unqualified) with (preprocess (action ...))
--------------------------------------------------------------------

  $ dune exec ./main.exe @all
  print_endline "foo"
