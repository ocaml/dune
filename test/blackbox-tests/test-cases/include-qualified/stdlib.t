Libraries designed as the "stdlib" may not use (include_subdirs qualified)

Although it's just an artificial limitation and when we can lift it if the need
arises to.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using experimental_building_ocaml_compiler_with_dune 0.1) 
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (stdlib))
  > EOF

  $ dune build @check
  File "dune", line 1, characters 0-27:
  1 | (include_subdirs qualified)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: a library with (stdlib ...) may not use (include_subdirs qualified)
  [1]
