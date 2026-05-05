Pforms are not expanded in (env-vars) fields, so %{pkg:...} is rejected:

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

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
