We are dropping support for findlib in dune

  $ dune build --root in-dune target.txt
  Entering directory 'in-dune'
  File "dune", line 2, characters 23-37:
  2 | (write-file target.txt %{findlib:pkg})
                             ^^^^^^^^^^^^^^
  Error: %{findlib:..} was renamed to '%{lib:..}' in the 1.0 version of the
  dune language
  Leaving directory 'in-dune'
  [1]
