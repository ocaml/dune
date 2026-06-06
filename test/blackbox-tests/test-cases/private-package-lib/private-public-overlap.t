We should forbid private libraries that belong to a package from depending on a
private library

  $ make_dune_project_with_package 3.5 pkg

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (libraries bar)
  >  (package pkg)
  >  (modules))
  > (library
  >  (name bar)
  >  (modules))
  > EOF

  $ dune build @install
  File "dune", line 3, characters 12-15:
  3 |  (libraries bar)
                  ^^^
  Error: Library "bar" is private, it cannot be a dependency of a private
  library attached to a package. You need to give "bar" a public name.
  [1]
