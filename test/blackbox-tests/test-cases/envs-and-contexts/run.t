Regression test for https://github.com/ocaml/dune/issues/1016#issuecomment-411390740

  $ dune printenv
  Environment for context dev:
    (
     (flags
      (-w
       @a-4-29-40-41-42-44-45-48-58-59-60-40
       -strict-sequence
       -strict-formats
       -short-paths
       -keep-locs))
     (ocamlc_flags (-g))
     (ocamlopt_flags (-g))
    )
  Environment for context release:
    (
     (flags (-w -40))
     (ocamlc_flags (-g))
     (ocamlopt_flags (-g))
    )
