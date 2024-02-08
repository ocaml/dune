Demonstrate what happens when we try to attach a library to a package that
doesn't exist:

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes byte))
  > EOF

  $ dune build @install
  File "dune", line 2, characters 14-17:
  2 |  (public_name foo)
                    ^^^
  Error: You cannot declare items to be installed without adding a
  <package>.opam file at the root of your project.
  To declare elements to be installed as part of package "foo", add a
  "foo.opam" file at the root of your project.
  Root of the project as discovered by dune: .
  [1]
Now we try to add a bad package:

  $ echo "(package (name bar))" >> dune-project

  $ dune build @install
  File "dune", line 2, characters 14-17:
  2 |  (public_name foo)
                    ^^^
  Error: The current scope doesn't define package "foo".
  The only packages for which you can declare elements to be installed in this
  directory are:
  - bar (because of bar.opam)
  [1]

Now we use another form instead of a library
  $ cat >dune <<EOF
  > (install
  >  (files dune)
  >  (section share)
  >  (package foo))
  > EOF

  $ dune build @install
  File "dune", line 4, characters 1-14:
  4 |  (package foo))
       ^^^^^^^^^^^^^
  Error: The current scope doesn't define package "foo".
  The only packages for which you can declare elements to be installed in this
  directory are:
  - bar (because of bar.opam)
  [1]

Same thing but without packages in the project

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ dune build @install
  File "dune", line 4, characters 1-14:
  4 |  (package foo))
       ^^^^^^^^^^^^^
  Error: You cannot declare items to be installed without adding a
  <package>.opam file at the root of your project.
  To declare elements to be installed as part of package "foo", add a
  "foo.opam" file at the root of your project.
  Root of the project as discovered by dune: .
  [1]
