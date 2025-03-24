Test the (dialect ...) stanza inside the `dune-project` file.

  $ dune exec ./main.exe
  File "dune-project", lines 3-7, characters 0-92:
  3 | (dialect
  4 |  (name mlfi)
  5 |  (implementation
  6 |   (extension mf)
  7 |   (format (run fmt %{input-file}))))
  Error: omitting (interface) in dialects is only available since version 3.9
  of the dune language. Please update your dune-project file to have (lang dune
  3.9).
  [1]

  $ dune build @fmt
  File "dune-project", lines 3-7, characters 0-92:
  3 | (dialect
  4 |  (name mlfi)
  5 |  (implementation
  6 |   (extension mf)
  7 |   (format (run fmt %{input-file}))))
  Error: omitting (interface) in dialects is only available since version 3.9
  of the dune language. Please update your dune-project file to have (lang dune
  3.9).
  [1]
