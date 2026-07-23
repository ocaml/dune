Pforms are not expanded in (env-vars) fields, so %{pkg:...} is rejected:

  $ make_dune_project_with_package 3.24 foo

  $ mkdir -p foo
  $ cat >foo/dune <<EOF
  > (install (section share) (package foo) (files (src.txt as dest.txt)))
  > EOF
  $ cat >foo/src.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (env-vars
  >    (MY_FILE %{pkg:foo:share:dest.txt}))))
  > EOF

  $ dune build 2>&1
  File "dune", line 4, characters 12-37:
  4 |    (MY_FILE %{pkg:foo:share:dest.txt}))))
                  ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]
