Reproduction case for #1529: using an extension when no dune-project
file is present.

  $ dune build @install 2>&1 | sed "s/(lang dune .*)/(lang dune <version>)/" | sed "s/(using menhir .*)/(using menhir <version>)/"
  File ".", line 1, characters 0-0:
  Warning: No dune-project file has been found in directory ".". A default one
  is assumed but the project might break when dune is upgraded. Please create a
  dune-project file.
  Hint: generate the project file with: $ dune init project <name>
  File "dune", line 1, characters 0-25:
  1 | (menhir (modules parser))
      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'menhir' is available only when menhir is enabled in the dune-project
  file. You must enable it using (using menhir <version>) in your dune-project file.
