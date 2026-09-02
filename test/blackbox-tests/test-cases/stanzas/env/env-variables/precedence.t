This test checks that the environment variables defined in (env) are set during
execution, and tests the precedence of the context vars vs global vars.

  $ cat >dune-project <<EOF
  > (lang dune 1.1)
  > EOF

Create dune-workspace file with global and context env vars set.

  $ cat >dune-workspace <<EOF
  > (lang dune 1.5)
  > (env
  > (_
  > (env-vars
  >  (VARIABLE_FROM_WORKSPACE value1)
  >  (VARIABLE_FROM_BOTH from_workspace))))
  > (context
  > (default
  >  (env
  >   (_
  >    (env-vars
  >     (VARIABLE_FROM_CONTEXT value2)
  >     (VARIABLE_FROM_BOTH from_context))))))
  > EOF

They can be set from the workspace:

  $ dune exec -- dune_cmd printenv VARIABLE_FROM_WORKSPACE
  VARIABLE_FROM_WORKSPACE=value1

From a (context) stanza in the workspace:

  $ dune exec -- dune_cmd printenv VARIABLE_FROM_CONTEXT
  VARIABLE_FROM_CONTEXT=value2

When a variable is set from both a context and a global one, the context one is
used.

  $ dune exec -- dune_cmd printenv VARIABLE_FROM_BOTH
  VARIABLE_FROM_BOTH=from_context
