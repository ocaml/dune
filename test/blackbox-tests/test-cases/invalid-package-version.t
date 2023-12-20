Invalid package names are reported by Dune.
We also give a hint for common invalid versions such as foo.1.2.3,
which can result from a users false assumption from opam.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name invalid)
  >  (allow_empty)
  >  (depends foo.1.2.3))
  > EOF

Building doesn't work as the dependency is invalid.

  $ dune build
  File "dune-project", line 5, characters 10-19:
  5 |  (depends foo.1.2.3))
                ^^^^^^^^^
  Error: "foo.1.2.3" is an invalid package dependency.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: (foo (= 1.2.3)) would be a correct package dependency
  [1]

We can take this further and add some really invalid characters into the name:

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name invalid)
  >  (allow_empty)
  >  (depends f{oo."1.2.3"))
  > (generate_opam_files true)
  > EOF

  $ dune build invalid.opam
  File "dune-project", line 5, characters 10-15:
  5 |  (depends f{oo."1.2.3"))
                ^^^^^
  Error: "f{oo." is an invalid package dependency.
  Package names can contain letters, numbers, '-', '_' and '+', and need to
  contain at least a letter.
  Hint: f_oo_ would be a correct package dependency
  [1]
