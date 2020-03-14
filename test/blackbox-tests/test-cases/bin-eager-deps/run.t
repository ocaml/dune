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
  $ dune build @install --display short
  File "dune", line 3, characters 9-32:
  3 |  (deps %{bin:doesnotexistbinary})
               ^^^^^^^^^^^^^^^^^^^^^^^
  Error: Program doesnotexistbinary not found in the tree or in PATH
   (context: default)
  [1]
