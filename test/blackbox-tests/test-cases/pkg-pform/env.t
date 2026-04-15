%{pkg:...} is not allowed in (env) stanzas.

The env-vars parser doesn't expand pforms at all:

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir -p foo
  $ cat >foo/dune <<EOF
  > (executable
  >  (name foo_bin)
  >  (public_name foo-bin)
  >  (package foo))
  > EOF
  $ cat >foo/foo_bin.ml <<EOF
  > let () = print_endline "I am foo"
  > EOF

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (env-vars
  >    (MY_SHARE %{pkg:foo:share}))))
  > EOF

  $ dune build 2>&1
  File "dune", line 4, characters 13-29:
  4 |    (MY_SHARE %{pkg:foo:share}))))
                   ^^^^^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

The (binaries) field does expand pforms, but %{pkg:...} introduces deps and
is rejected in this no-deps context:

  $ cat >dune <<EOF
  > (env
  >  (_
  >   (binaries (%{pkg:foo:bin}/foo-bin as my-tool))))
  > (rule
  >  (alias test-env-bin)
  >  (action (run my-tool)))
  > EOF

  $ dune build @test-env-bin 2>&1
  File "dune", line 3, characters 13-27:
  3 |   (binaries (%{pkg:foo:bin}/foo-bin as my-tool))))
                   ^^^^^^^^^^^^^^
  Error: %{pkg:..} isn't allowed in this position.
  [1]
