  $ dune build --display short --profile release | grep "FLG -ppx"
  File "dune", line 4, characters 13-23:
  4 |  (preprocess (pps ppx1)))
                   ^^^^^^^^^^
  Warning: .merlin generated is inaccurate. pps specification isn't identical
  in all stanzas.
  Split the stanzas into different directories or silence this warning by
  adding (allow_approximate_merlin) to your dune-project.
  [1]
