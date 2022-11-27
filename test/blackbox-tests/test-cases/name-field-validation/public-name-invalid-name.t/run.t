there's only a public name but it's invalid as a name

  $ dune build
  File "dune", line 1, characters 22-28:
  1 | (library (public_name c.find))
                            ^^^^^^
  Error: Invalid library name.
  Public library names don't have this restriction. You can either change this
  public name to be a valid library name or add a "name" field with a valid
  library name.
  Hint: Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: c_find would be a correct library name
  [1]

