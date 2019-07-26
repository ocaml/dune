We are dropping support for findlib in dune

  $ dune build --root in-dune target.txt
  Entering directory 'in-dune'
  File "dune", line 2, characters 25-37:
  2 | (write-file target.txt %{findlib:pkg})
                               ^^^^^^^^^^^^
  Error: %{findlib:..} was renamed to '%{lib:..}' in the 1.0 version of the
  dune language
  [1]

But it must still be available in jbuild files

  $ dune build --root in-jbuild target.txt
  Entering directory 'in-jbuild'
  File "jbuild", line 1, characters 0-0:
  Warning: jbuild files are not allowed inside Dune 2.0 projects, please
  convert this file to a dune file instead.
  Note: You can use "dune upgrade" to convert your project to dune.
  File "jbuild", line 4, characters 23-42:
  4 | (write-file target.txt ${findlib:pkg:file})
                             ^^^^^^^^^^^^^^^^^^^
  Error: Library "pkg" not found.
  Hint: try: dune external-lib-deps --missing --root in-jbuild target.txt
  [1]
