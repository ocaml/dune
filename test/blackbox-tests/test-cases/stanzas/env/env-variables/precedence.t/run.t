Environment variables defined in (env) are set during execution.

They can be set from the workspace:

  $ dune exec ./printenv.exe VARIABLE_FROM_WORKSPACE
  VARIABLE_FROM_WORKSPACE=value1

From a (context) stanza in the workspace:

  $ dune exec ./printenv.exe VARIABLE_FROM_CONTEXT
  VARIABLE_FROM_CONTEXT=value2

When a variable is set from both a context and a global one, the context one is
used.

  $ dune exec ./printenv.exe VARIABLE_FROM_BOTH
  VARIABLE_FROM_BOTH=from_workspace
