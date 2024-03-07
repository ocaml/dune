01_module is not a valid module name, this should be detected before trying to `dune build`:

  $ dune init project 01_module
  Entering directory '01_module'
  Error: "01_module" is an invalid module name.
  Module names must be non-empty, start with a letter, and composed only of the
  following characters: 'A'..'Z', 'a'..'z', '_', ''' or '0'..'9'.
  Hint: M01_module would be a correct module name
  Leaving directory '01_module'
  [1]
