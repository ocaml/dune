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
  Error: Unable to run ocamllsp as a dev-tool because you don't appear to be
  inside a dune project.
  [1]
