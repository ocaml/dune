We test the `(select)` field of the `(libraries)` field in the presence of
`(include_subdirs unqualified)`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> bar.unix.ml)
  >    (!unix -> bar.nounix.ml))))
  > EOF

  $ mkdir -p sub
  $ cat > sub/bar.unix.ml <<EOF
  > let () = print_endline "Test: Unix was found!"
  > EOF
  $ cat > sub/bar.nounix.ml <<EOF
  > let () = print_endline "Test: Unix was not found!"
  > EOF

The select field does not pick up the module sources for the test stanza
correctly. This is a bug.

  $ dune build foo.cma
  File "_unknown_", line 1, characters 0-0:
  Error: No rule found for bar.unix.ml
  [1]

  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select bar.ml from
  >    (unix -> sub/bar.unix.ml)
  >    (!unix -> sub/bar.nounix.ml))))
  > EOF

It also doesn't allow specifying the path

  $ dune build foo.cma
  File "dune", line 6, characters 12-27:
  6 |    (unix -> sub/bar.unix.ml)
                  ^^^^^^^^^^^^^^^
  Error: The format for files in this select branch must be bar.{name}.ml
  [1]
