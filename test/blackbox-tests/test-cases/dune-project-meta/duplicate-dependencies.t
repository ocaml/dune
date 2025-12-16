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
  Warning: Duplicate dependency on package ocaml in 'depends' field. If you
  want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  Warning: Duplicate dependency on package (ocaml (>= 4.08)) in 'depends'
  field. If you want to specify multiple constraints, combine them using (and
  ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

Same package with different constraints should warn
----------------------------------------------------

Multiple constraints on the same package should be combined using (and ...).

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (name test-pkg)
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends (ocaml (>= 4.08)) (ocaml (< 5.0))))
  > EOF

  $ dune build
  File "dune-project", line 6, characters 28-43:
  6 |  (depends (ocaml (>= 4.08)) (ocaml (< 5.0))))
                                  ^^^^^^^^^^^^^^^
  Warning: Duplicate dependency on package (ocaml (< 5.0)) in 'depends' field.
  If you want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  File "dune-project", line 6, characters 27-31:
  6 |  (depends ocaml ocaml dune dune))
                                 ^^^^
  Warning: Duplicate dependency on package dune in 'depends' field. If you want
  to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))
  File "dune-project", line 6, characters 16-21:
  6 |  (depends ocaml ocaml dune dune))
                      ^^^^^
  Warning: Duplicate dependency on package ocaml in 'depends' field. If you
  want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  Warning: Duplicate dependency on package base in 'conflicts' field. If you
  want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  Warning: Duplicate dependency on package lwt in 'depopts' field. If you want
  to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

Mix of valid and invalid duplicates
------------------------------------

Even different constraints on the same package are now warned about.

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
  Warning: Duplicate dependency on package (dune (>= 2.0)) in 'depends' field.
  If you want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))
  File "dune-project", line 8, characters 2-17:
  8 |   (ocaml (< 5.0))
        ^^^^^^^^^^^^^^^
  Warning: Duplicate dependency on package (ocaml (< 5.0)) in 'depends' field.
  If you want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  File "dune-project", line 6, characters 22-27:
  6 |  (depends ocaml ocaml ocaml))
                            ^^^^^
  Warning: Duplicate dependency on package ocaml in 'depends' field. If you
  want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))
  File "dune-project", line 6, characters 16-21:
  6 |  (depends ocaml ocaml ocaml))
                      ^^^^^
  Warning: Duplicate dependency on package ocaml in 'depends' field. If you
  want to specify multiple constraints, combine them using (and ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

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
  Warning: Duplicate dependency on package (alcotest :with-test) in 'depends'
  field. If you want to specify multiple constraints, combine them using (and
  ...).
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (duplicate_deps disabled))

Disabling the warning
---------------------

The warning can be disabled using the (warnings ...) field in dune-project.

  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (name test-pkg)
  > (warnings (duplicate_deps disabled))
  > (package
  >  (name test-pkg)
  >  (allow_empty)
  >  (depends ocaml ocaml))
  > EOF

  $ dune build
