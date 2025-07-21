Exercise the behaviour of "dune tools exec ocamllsp" when run outside
of a dune project.


This is necessary for dune to act as it normally would outside of a
dune workspace.
  $ unset INSIDE_DUNE

Run the wrapper command from a temporary directory. With INSIDE_DUNE
unset dune would otherwise pick up the dune project itself as the
current workspace.
  $ cd $(mktemp -d)

  $ dune tools exec ocamllsp
  Error: I cannot find the root of the current workspace/project.
  If you would like to create a new dune project, you can type:
  
      dune init project NAME
  
  Otherwise, please make sure to run dune inside an existing project or
  workspace. For more information about how dune identifies the root of the
  current workspace/project, please refer to
  https://dune.readthedocs.io/en/stable/usage.html#finding-the-root
  [1]
