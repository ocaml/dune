Expansion of enabled_if in the cram stanza

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

We define a test that is disabled by default but can be turned using the FOO
env var.

  $ cat >dune <<EOF
  > (cram
  >  (applies_to :whole_subtree)
  >  (enabled_if %{env:FOO=false}))
  > EOF

  $ mkdir sub

We set the FOO env var in the sub directory of the test, but this shouldn't
have an effect because environment variable should be expanded where the cram
stanza was defined.

  $ cat >sub/dune <<EOF
  > (env
  >  (_
  >   (env-vars (FOO true))))
  > EOF

  $ cat >sub/foo.t <<EOF
  >   $ echo should be disabled
  > EOF

  $ dune runtest
