----------------------------------------------------------------------------------
Testsuite for https://github.com/ocaml/dune/issues/3043
`dune init` should successfully create lib or exec projects with library stanza 
when --libs option is provided.
----------------------------------------------------------------------------------

`dune init exe main --libs="lwt,lwt.unix"` returns a success message

  $ dune init exe main --libs="lwt,lwt.unix"
  Success: initialized executable component named main

`dune init exe exe1 --libs="lwt,lwt-unix"` returns a success message

  $ dune init exe exe1 --libs="lwt,lwt-unix"
  Success: initialized executable component named exe1

