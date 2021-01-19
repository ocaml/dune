Creating an invalid (deprecated_library_name ..) is an error

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name a))
  > EOF

  $ cat >dune <<EOF
  > (deprecated_library_name (old_public_name a) (new_public_name a))
  > EOF

  $ dune build @all
  File "dune", line 1, characters 42-43:
  1 | (deprecated_library_name (old_public_name a) (new_public_name a))
                                                ^
  Error: old_public_name cannot be the same as the new_public_name
  [1]
