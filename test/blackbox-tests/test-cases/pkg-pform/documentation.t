%{pkg:...} resolves files installed by documentation stanzas.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (documentation (package foo))
  > EOF

  $ cat >foo.mld <<EOF
  > {0 Foo}
  > EOF

The odoc-pages path resolves to the source .mld file:

  $ mkdir -p test

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-doc)
  >  (action (echo "%{pkg:foo:doc:odoc-pages/foo.mld}\n")))
  > EOF

  $ dune build @test-doc 2>&1
  ../foo.mld
