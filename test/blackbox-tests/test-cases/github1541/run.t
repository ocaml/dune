This demonstrates error messages whennever a pform isn't expanded

  $ echo '(lang dune 2.0)' > dune-project

for libraries:

  $ echo '(rule (with-stdout-to dummy1 (echo "%{lib:fakelib:bar.ml}")))' > dune
  $ dune build ./dummy1
  File "dune", line 1, characters 38-57:
  1 | (rule (with-stdout-to dummy1 (echo "%{lib:fakelib:bar.ml}")))
                                            ^^^^^^^^^^^^^^^^^^^
  Error: Library "fakelib" not found.
  Hint: try: dune external-lib-deps --missing ./dummy1
  [1]

for binaries:

  $ echo '(rule (with-stdout-to dummy2 (echo "%{bin:fakebin}")))' > dune
  $ dune build ./dummy2
  File "dune", line 1, characters 38-50:
  1 | (rule (with-stdout-to dummy2 (echo "%{bin:fakebin}")))
                                            ^^^^^^^^^^^^
  Error: Program fakebin not found in the tree or in PATH
   (context: default)
  [1]
