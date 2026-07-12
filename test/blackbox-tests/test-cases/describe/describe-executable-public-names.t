`dune describe` reports the public name of an executable when it has one
========================================================================

  $ make_dune_project_with_package 3.21 mypkg

A single executable with a public name:

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (public_name my-tool)
  >  (package mypkg))
  > EOF

  $ touch main.ml

`dune describe` reports the public name rather than the private name:

  $ dune describe | grep -A 2 '(executables'
   (executables
    ((names (my-tool))
     (requires ())

Multiple executables, where only some have a public name (a public name of
`-` means no public name for that executable):

  $ cat >dune <<EOF
  > (executables
  >  (names a b c)
  >  (public_names x - z)
  >  (package mypkg))
  > EOF

  $ touch a.ml b.ml c.ml

The private name is used as a fallback when there is no public name:

  $ dune describe | grep -A 3 '(executables'
   (executables
    ((names
      (x b z))
     (requires ())
