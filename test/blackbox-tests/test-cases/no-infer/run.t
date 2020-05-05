An action with source dependencies that are generated outside of dune should work if wrapped in (no-infer ...) but not otherwise.

  $ dune build --root error-without-no-infer
  Entering directory 'error-without-no-infer'
  File "dune", line 1, characters 0-80:
  1 | (rule
  2 | (targets target)
  3 | (action (progn (run touch source) (copy source target))))
  Error: No rule found for source
  [1]

  $ dune build --root no-error-with-no-infer
  Entering directory 'no-error-with-no-infer'
