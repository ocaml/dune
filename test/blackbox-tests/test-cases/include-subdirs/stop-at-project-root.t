include_subdirs should not recurse past project root

  $ cat >dune-project <<EOF
  > (lang dune 3.1)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > print_endline Bar.v;;
  > EOF

  $ mkdir subproj
  $ cat >subproj/bar.ml <<EOF
  > let v = "unusable"
  > EOF

Works if [subproj] is just a separate directory and not a separate project.

  $ dune exec ./foo.exe
  unusable

  $ cat >subproj/dune-project <<EOF
  > (lang dune 3.1)
  > EOF

Doesn't work with when we make [subproj] a separate project with a dune-project
file, since include_subdirs is stopped.

  $ dune exec ./foo.exe
  File "foo.ml", line 1, characters 14-19:
  1 | print_endline Bar.v;;
                    ^^^^^
  Error: Unbound module Bar
  [1]
