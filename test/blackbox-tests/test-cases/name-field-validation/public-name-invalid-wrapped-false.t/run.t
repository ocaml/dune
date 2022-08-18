there's only a public name which is invalid, and the library is unwrapped
this is an error since 2.0.0, it was only a warning before

  $ dune build
  File "dune", line 3, characters 14-21:
  3 |  (public_name foo.bar))
                    ^^^^^^^
  Error: Invalid library name.
  Public library names don't have this restriction. You can either change this
  public name to be a valid library name or add a "name" field with a valid
  library name.
  Hint: Library names must be non-empty and composed only of the following
  characters: 'A'..'Z', 'a'..'z', '_' or '0'..'9'.
  Hint: foo_bar would be a correct library name
  [1]
