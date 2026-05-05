%{pkg:...} resolves files installed by Rocq theories.

The compiled .vo file resolves to the build artifact:

  $ mkdir -p test

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-vo)
  >  (action (echo "%{pkg:foo:lib_root:coq/user-contrib/basic/foo.vo}\n")))
  > EOF

  $ dune build @test-vo 2>&1
  ../foo.vo

The source .v file should also resolve:

  $ cat >test/dune <<EOF
  > (rule
  >  (alias test-v)
  >  (action (echo "%{pkg:foo:lib_root:coq/user-contrib/basic/foo.v}\n")))
  > EOF

  $ dune build @test-v 2>&1
  ../foo.v
