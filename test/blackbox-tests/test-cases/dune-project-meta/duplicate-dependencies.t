Duplicate dependencies detection
=================================

Exact duplicate without constraints should warn
------------------------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends ocaml ocaml))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 16-21:
  6 |  (depends ocaml ocaml))
                      ^^^^^
  Warning: Duplicate dependency ocaml in 'depends' field.
  Hint: Remove one of the duplicate entries.

Exact duplicate with same constraint should warn
-------------------------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends (ocaml (>= 4.08)) (ocaml (>= 4.08))))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 28-45:
  6 |  (depends (ocaml (>= 4.08)) (ocaml (>= 4.08))))
                                  ^^^^^^^^^^^^^^^^^
  Warning: Duplicate dependency (ocaml (>= 4.08)) in 'depends' field.
  Hint: Remove one of the duplicate entries.

Same package with different constraints is allowed
---------------------------------------------------

This is valid opam syntax for specifying version ranges.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends (ocaml (>= 4.08)) (ocaml (< 5.0))))
  > EOF

  $ dune build

Multiple duplicates in the same field
--------------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends ocaml ocaml dune dune))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 16-21:
  6 |  (depends ocaml ocaml dune dune))
                      ^^^^^
  Warning: Duplicate dependency ocaml in 'depends' field.
  Hint: Remove one of the duplicate entries.
  File "dune-project", line 6, characters 27-31:
  6 |  (depends ocaml ocaml dune dune))
                                 ^^^^
  Warning: Duplicate dependency dune in 'depends' field.
  Hint: Remove one of the duplicate entries.

Duplicates in conflicts field
------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (conflicts base base))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 17-21:
  6 |  (conflicts base base))
                       ^^^^
  Warning: Duplicate dependency base in 'conflicts' field.
  Hint: Remove one of the duplicate entries.

Duplicates in depopts field
----------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depopts lwt lwt))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 14-17:
  6 |  (depopts lwt lwt))
                    ^^^
  Warning: Duplicate dependency lwt in 'depopts' field.
  Hint: Remove one of the duplicate entries.

Mix of valid and invalid duplicates
------------------------------------

Different constraints on the same package are valid, but exact duplicates are not.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends
  >   (ocaml (>= 4.08))
  >   (ocaml (< 5.0))
  >   (dune (>= 2.0))
  >   (dune (>= 2.0))))
  > EOF

  $ dune build
  File "dune-project", line 10, characters 2-17:
  10 |   (dune (>= 2.0))))
         ^^^^^^^^^^^^^^^
  Warning: Duplicate dependency (dune (>= 2.0)) in 'depends' field.
  Hint: Remove one of the duplicate entries.

Triple duplicate
----------------

When the same dependency appears three times, each duplicate is reported.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends ocaml ocaml ocaml))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 16-21:
  6 |  (depends ocaml ocaml ocaml))
                      ^^^^^
  Warning: Duplicate dependency ocaml in 'depends' field.
  Hint: Remove one of the duplicate entries.
  File "dune-project", line 6, characters 22-27:
  6 |  (depends ocaml ocaml ocaml))
                            ^^^^^
  Warning: Duplicate dependency ocaml in 'depends' field.
  Hint: Remove one of the duplicate entries.

Duplicate with filter constraint
---------------------------------

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends (alcotest :with-test) (alcotest :with-test)))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 32-53:
  6 |  (depends (alcotest :with-test) (alcotest :with-test)))
                                      ^^^^^^^^^^^^^^^^^^^^^
  Warning: Duplicate dependency (alcotest :with-test) in 'depends' field.
  Hint: Remove one of the duplicate entries.
