Dash are allowed in project names and should be accepted:

  $ dune init project dash-exe
  Entering directory 'dash-exe'
  Success: initialized project component named dash-exe
  Leaving directory 'dash-exe'
  $ cd dash-exe && dune build

  $ dune init project dash-lib --kind=library
  Entering directory 'dash-lib'
  Success: initialized project component named dash-lib
  Leaving directory 'dash-lib'
  $ cd dash-lib && dune build

Invalid project names should still be rejected:

  $ dune init project invalid.name
  dune: NAME argument: invalid project name `invalid.name'
        Project names must start with a letter and be composed only of
        letters,
        numbers, '-' or '_'
  Usage: dune init project [OPTION]… NAME [PATH]
  Try 'dune init project --help' or 'dune --help' for more information.
  [1]
  $ dune_cmd exists invalid.name
  false
