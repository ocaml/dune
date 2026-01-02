We test the `(select)` field of the `(libraries)` field in the presence of
`(include_subdirs qualified)`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (include_subdirs qualified)
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
  File "dune", lines 5-7, characters 2-75:
  5 |   (select bar.ml from
  6 |    (unix -> bar.unix.ml)
  7 |    (!unix -> bar.nounix.ml))))
  Error: No rule found for bar.unix.ml
  [1]

  $ cat > dune <<EOF
  > (include_subdirs qualified)
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

Specifying the relative filename works

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (libraries
  >   (select sub/bar.ml from
  >    (unix -> sub/bar.unix.ml)
  >    (!unix -> sub/bar.nounix.ml))))
  > EOF
  $ cat > foo.ml <<EOF
  > module X = Sub.Bar
  > EOF

Paths must be specified relative to the module group root

  $ dune build foo.cma
  Error: foo__Sub/bar corresponds to an invalid module name
  -> required by _build/default/foo__.ml-gen
  -> required by _build/default/.foo.objs/byte/foo__.cmo
  -> required by _build/default/foo.cma
  [1]

Works with executables as well:

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (executable
  >  (name foo)
  >  (libraries
  >   (select sub/bar.ml from
  >    (unix -> sub/bar.unix.ml)
  >    (!unix -> sub/bar.nounix.ml))))
  > EOF

  $ dune exec ./foo.exe
  Error: dune__exe__Sub/bar corresponds to an invalid module name
  -> required by _build/default/.foo.eobjs/dune__exe.ml-gen
  -> required by _build/default/.foo.eobjs/byte/dune__exe.cmi
  -> required by _build/default/.foo.eobjs/native/dune__exe.cmx
  -> required by _build/default/foo.exe
  [1]
