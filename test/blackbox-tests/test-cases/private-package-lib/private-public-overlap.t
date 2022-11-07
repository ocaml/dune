We should forbid private libraries that belong to a package from depending on a
private library

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name pkg))
  > EOF

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
