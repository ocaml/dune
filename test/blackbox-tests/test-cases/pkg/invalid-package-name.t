Test that we detect errors when invalid package names are used in various
places.

  $ . ./helpers.sh

  $ make_lockdir
  $ add_mock_repo_if_needed

Invalid name of local package:
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name a.b))
  > EOF
  $ dune pkg lock
  File "dune-project", line 2, characters 15-18:
  2 | (package (name a.b))
                     ^^^
  Error: "a.b" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: a_b would be a correct opam package name
  [1]

Invalid name of dependency:
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name a)
  >  (depends b.c))
  > EOF
  $ dune pkg lock
  File "dune-project", line 4, characters 10-13:
  4 |  (depends b.c))
                ^^^
  Error: "b.c" is an invalid package dependency.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: (b (= c)) would be a correct package dependency
  [1]

  $ rm dune-project

Invalid name of lockfile:
  $ cat >dune.lock/c.d.pkg <<EOF
  > (version 0.0.1)
  > EOF
  $ build_pkg c.d
  Error: "c.d" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: c_d would be a correct opam package name
  Hint: The lockfile "c.d.pkg" is named incorrectly. Lockfiles must be named
  <package>.pkg where <package> is a valid opam package name.
  [1]

  $ rm dune.lock/c.d.pkg

Invalid dependency in lockfile:
  $ cat >dune.lock/x.pkg <<EOF
  > (version 0.0.1)
  > (deps d.e)
  > EOF
  $ build_pkg x
  File "dune.lock/x.pkg", line 2, characters 6-9:
  2 | (deps d.e)
            ^^^
  Error: "d.e" is an invalid opam package name.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: d_e would be a correct opam package name
  [1]

