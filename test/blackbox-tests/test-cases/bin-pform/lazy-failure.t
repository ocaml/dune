Reproduces issue #3252

https://github.com/ocaml/dune/issues/3252

If a rule requires an expansion that introduces a failure, we should fail only
when the rule needs to be used to build a target.

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name randompackage))
  > EOF
  $ cat >dune <<EOF
  > (rule
  >  (targets testfile)
  >  (deps %{bin:doesnotexistbinary})
  >  (action (echo "test")))
  > EOF

@install does not depend on testfile, so the missing bin pform does
not fire:

  $ dune build @install

Asking for testfile directly does fire the expansion:

  $ dune build testfile
  File "dune", line 3, characters 7-32:
  3 |  (deps %{bin:doesnotexistbinary})
             ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Program doesnotexistbinary not found in the tree or in PATH
   (context: default)
  [1]
