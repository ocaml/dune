%{pkg:...} resolves files installed by public libraries.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (public_name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let x = 42
  > EOF

The .cma archive resolves to the build output:

  $ mkdir -p test

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-lib)
  >  (action (echo "%{pkg:foo:lib:foo.cma}\n")))
  > EOF

  $ dune build @test-lib 2>&1
  ../foo.cma

The .cmi interface file should also resolve:

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-cmi)
  >  (action (echo "%{pkg:foo:lib:foo.cmi}\n")))
  > EOF

  $ dune build @test-cmi 2>&1
  ../.foo.objs/byte/foo.cmi

The library archive appears as a build dependency:

  $ cat >test/dune <<EOF
  > (rule
  >  (target out)
  >  (action (with-stdout-to %{target} (echo "%{pkg:foo:lib:foo.cma}"))))
  > EOF

  $ dune rules --format=json _build/default/test/out 2>&1 | jq 'include "dune"; .[] | ruleDepFilePaths' | sort
  "_build/default/foo.cma"
