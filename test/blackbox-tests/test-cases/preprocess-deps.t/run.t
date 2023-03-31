Running the build should succeed as `gen/gen.exe` is built to preprocess
`f.ml`, but it doesn't, as `gen/gen.ml` is in `src/` and not in the workspace
root:

  $ dune build
  File "src/.merlin-conf/_unknown_", line 1, characters 0-0:
  Error: No rule found for gen/gen.exe
  [1]

