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
  $ touch sub/bar.unix.mli sub/bar.nounix.mli

The select field does not pick up the module sources for the test stanza
correctly. This is a bug.

  $ dune build foo.cma
  File "dune", lines 5-7, characters 2-75:
  5 |   (select bar.ml from
  6 |    (unix -> bar.unix.ml)
  7 |    (!unix -> bar.nounix.ml))))
  Error: No rule found for bar.unix.ml
  [1]

  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select sub/bar.ml from
  >    (unix -> sub/bar.unix.ml)
  >    (!unix -> sub/bar.nounix.ml))))
  > EOF

It also doesn't allow specifying the path

  $ dune build foo.cma

  $ cat > dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select ./sub/bar.ml from
  >    (unix -> sub/bar.unix.ml)
  >    (!unix -> sub/bar.nounix.ml))
  >   (select sub/bar.mli from
  >    (unix -> sub/bar.unix.mli)
  >    (!unix -> sub/bar.nounix.mli))))
  > EOF

  $ dune build foo.cma

