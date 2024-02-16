Demonstrate an optional executable available from more than one definition

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF

  $ mkdir a b
  $ touch a/foo.ml b/foo.ml
  $ cat >a/dune <<EOF
  > (executable
  >  (public_name foo)
  >  (enabled_if true))
  > EOF
  $ cat >b/dune <<EOF
  > (executable
  >  (public_name foo)
  >  (enabled_if true))
  > EOF
  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run %{bin:foo})))
  > EOF

  $ dune build @foo
  File "b/dune", line 2, characters 14-17:
  2 |  (public_name foo)
                    ^^^
  Error: binary "foo" is available from more than one definition. It is also
  available in:
  - a/dune:2
  [1]
