#7108: foo-bar is a valid public name, we should accept it.

  $ dune init lib foo_bar --public foo-bar
  dune: option '--public': invalid component name `foo-bar'
        Library names must be non-empty and composed only of the
        following
        characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Usage: dune init library [OPTION]… NAME [PATH]
  Try 'dune init library --help' or 'dune --help' for more information.
  [1]
