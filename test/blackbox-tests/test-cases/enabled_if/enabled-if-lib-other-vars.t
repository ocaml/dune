Here we demonstrate that we expand any variable in enabled_if on libraries

  $ make_dune_project 3.15

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (enabled_if %{env:FOO=false}))
  > EOF
  $ dune build %{cma:./foo}
  File "command line", line 1, characters 0-12:
  Error: Library foo does not exist.
  [1]
  $ FOO=true dune build %{cma:./foo}
