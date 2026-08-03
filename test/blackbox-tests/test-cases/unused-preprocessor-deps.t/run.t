`preprocessor_deps` is provided without `preprocess` and is ignored.
Should warn.

  $ touch a.ml b.ml

  $ make_dune_project 1.11
  $ dune build
  File "dune", line 5, characters 1-39:
  5 |  (preprocessor_deps does-not-exist.txt))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This preprocessor_deps field will be ignored because no preprocessor
  that might use them is configured.
  File "dune", line 11, characters 1-39:
  11 |  (preprocessor_deps does-not-exist.txt))
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This preprocessor_deps field will be ignored because no preprocessor
  that might use them is configured.

  $ make_dune_project 2.0
  $ dune build
  File "dune", line 5, characters 1-39:
  5 |  (preprocessor_deps does-not-exist.txt))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: This preprocessor_deps field will be ignored because no preprocessor
  that might use them is configured.
  [1]
