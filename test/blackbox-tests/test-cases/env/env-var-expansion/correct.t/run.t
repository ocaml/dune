Actually test that the environment changes are properly tracked, i.e. that
incrementality works properly, that (setenv ...) is taken into account, etc.

  $ dune build @echo1
  true
  $ DUNE_ENV_VAR=true dune build @echo1
  $ DUNE_ENV_VAR=false dune build @echo1
  false
  $ DUNE_ENV_VAR=false dune build @echo1
  $ DUNE_ENV_VAR=true dune build @echo1

This test is broken because previous/new values should differ in these tests. In
the dune file, the environment variable ends up being set locally, but this
isn't reflected on a per action basis.

  $ dune build @echo2
  previous env: unset
  new env:set by setenv
  $ DUNE_ENV_VAR=true dune build @echo2
  previous env: true
  new env:set by setenv
  $ DUNE_ENV_VAR=false dune build @echo2
  previous env: false
  new env:set by setenv

  $ dune build @enabled
  enabled!
  $ DUNE_ENV_VAR=true dune build @enabled
  $ DUNE_ENV_VAR=false dune build @enabled

  $ dune build @disabled
  $ DUNE_ENV_VAR=true dune build @disabled
  enabled!
  $ DUNE_ENV_VAR=false dune build @disabled
