Dune does not report an invalid module name as an error
  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ touch foo.ml foo-as-bar.ml
  $ dune build @all
  Error: exception { exn = ("Invalid Module_name.t", { s = "foo-as-bar" })
