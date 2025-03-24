Test the behavior of long lines

In the default output, user input affects on which lines which of the output
lines appear. For example, with a short project name, the line wraps later:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name short))
  > EOF
  $ dune build
  Error: The package short does not have any user defined stanzas attached to
  it. If this is intentional, add (allow_empty) to the package definition in
  the dune-project file
  -> required by _build/default/short.install
  -> required by alias all
  -> required by alias default
  [1]

With a long project name, there are less words on the line and thus it wraps in
a different position:

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name verylongnamethatcauseslinewrappingincases))
  > EOF
  $ dune build
  Error: The package verylongnamethatcauseslinewrappingincases does not have
  any user defined stanzas attached to it. If this is intentional, add
  (allow_empty) to the package definition in the dune-project file
  -> required by
     _build/default/verylongnamethatcauseslinewrappingincases.install
  -> required by alias all
  -> required by alias default
  [1]

Disabling line breaks should thus only break lines where there are explicit
line breaks in the input and not when the line gets too long.

  $ export DUNE_CONFIG__SKIP_LINE_BREAK=enabled
  $ dune build
  Error: The package verylongnamethatcauseslinewrappingincases does not have any user defined stanzas attached to it. If this is intentional, add (allow_empty) to the package definition in the dune-project file
  -> required by _build/default/verylongnamethatcauseslinewrappingincases.install
  -> required by alias all
  -> required by alias default
  [1]
